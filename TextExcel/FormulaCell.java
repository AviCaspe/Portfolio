package TextExcel;

import java.util.*;
import java.util.stream.Collectors;

public class FormulaCell extends RealCell {
    private String formula;
    private List<String> rpn;
    private Spreadsheet sheetRef;

    // super.doubleValue is never used, as getDoubleValue is overridden here
    public FormulaCell(String formula, Spreadsheet sheetRef) {
        this.formula = formula.toUpperCase();
        this.rpn = convertToRPN(this.formula);
        System.out.println(rpn.toString());
        this.sheetRef = sheetRef;
    }

    // Converts an equation string into rpn
    // RPN conversion is done with Edsger Dijkstra's Shunting Yard Algorithm
    private static List<String> convertToRPN(String formula) {
        String[] tokens = formula.split("\\s+");
        List<String> rpn = new ArrayList<>(tokens.length);

        // Higher value means higher precedence
        Map<String, Integer> precedence = new HashMap<>();
        precedence.put("(", 0);
        precedence.put(")", 0);
        precedence.put("+", 1);
        precedence.put("-", 1);
        precedence.put("*", 2);
        precedence.put("/", 2);
        precedence.put("SUM", 3);
        precedence.put("AVG", 3);
        // A stack to use for the rpn conversion
        Deque<String> opStack = new ArrayDeque<>();

        // Start of Shunting Yard Algorithm
        for (String token : tokens) {
            // Token is either a number, a cell reference or a range
            if (!isOperator(token)) {
                rpn.add(token);

            // Token is a operator
            } else {
                // If a paren is encountered, pop everything
                if (token.equals(")")) {
                    while (!opStack.peek().equals("(")) {
                        rpn.add(opStack.pop());
                    }
                    // pop the final paren
                    opStack.pop();
                } else {
                    while (!opStack.isEmpty() && precedence.get(token) <= precedence.get(opStack.peek())) {
                        rpn.add(opStack.pop());
                    }
                    opStack.push(token);
                }
            }
        }
        // clear out the remaining operators
        while (!opStack.isEmpty()) {
            rpn.add(opStack.pop());
        }

        return rpn;
    }

    @Override
    public double getDoubleValue() throws IllegalArgumentException {
        Deque<RealCell> callStack = new ArrayDeque<>();
        return this.getDoubleValue(callStack);
    }

    // Evaluates the formula stored in this cell
    @Override
    protected double getDoubleValue(Deque<RealCell> callStack) throws IllegalArgumentException {
        if (callStack.contains(this)) {
            throw new IllegalArgumentException();
        }
        callStack.push(this);

        Deque<String> evalStack = new ArrayDeque<>();
        for (String token : this.rpn) {
            // Next token is a number or cell reference
            if (!isOperator(token)) {
                evalStack.push(token);

            // Token is a operation
            } else {
                // Token is a method formula
                if (token.equalsIgnoreCase("sum") || token.equalsIgnoreCase("avg")) {
                    String range = evalStack.pop();
                    evalStack.push(Double.toString(this.methodFormula(range, token, callStack)));

                // Token is a mathematical operator
                } else {
                    double num2 = this.tokenToDouble(evalStack.pop(), callStack);
                    double num1 = this.tokenToDouble(evalStack.pop(), callStack);
                    evalStack.push(Double.toString(this.eval(num1, num2, token)));
                }
            }
        }

        double finalValue = tokenToDouble(evalStack.pop(), callStack);
        callStack.pop();
        return finalValue;
    }

    // Evaluates a two operand math expression
    private double eval(double num1, double num2, String op) throws IllegalArgumentException {
        double result = 0;
        switch (op) {
            case "+":
                result = num1 + num2;
                break;
            case "-":
                result = num1 - num2;
                break;
            case "*":
                result = num1 * num2;
                break;
            case "/":
                // Check for divide by zero
                if (num2 == 0) {
                    throw new IllegalArgumentException();
                }
                // Do the division
                result = num1 / num2;
                break;
        }
        return result;
    }

    // Evalutes a method forumula
    private double methodFormula(String range, String op, Deque<RealCell> callStack) throws IllegalArgumentException {
        String[] locs = range.split("-");
        Location ULCorner = new Location(locs[0]);
        Location DRCorner = new Location(locs[1]);
        double result = 0;
        if (op.equalsIgnoreCase("sum")) {
            result = this.sum(ULCorner, DRCorner, callStack);
        } else if (op.equalsIgnoreCase("avg")) {
            result = this.avg(ULCorner, DRCorner, callStack);
        }
        return result;
    }

    // Calculates the sum of a region
    private double sum(Location ULCorner, Location DRCorner, Deque<RealCell> callStack) throws IllegalArgumentException {
        return Spreadsheet.getLocsInRegion(ULCorner, DRCorner)
                          .stream()
                          .collect(Collectors.summingDouble(x -> this.locToDouble(x, callStack)));
    }

    // calculates the avg of a region
    private double avg(Location ULCorner, Location DRCorner, Deque<RealCell> callStack) throws IllegalArgumentException {
        return this.sum(ULCorner, DRCorner, callStack) / Spreadsheet.getRegionSize(ULCorner, DRCorner);
    }

    // Converts a token like "5", "13", "A1", or "b12" to the correct double value
    private double tokenToDouble(String token, Deque<RealCell> callStack) throws IllegalArgumentException {
        // Token is a number
        if (!token.matches("[A-Za-z][0-9][0-9]?")){
            return Double.parseDouble(token);
        }

        // Token is a cell reference
        return this.locToDouble(new Location(token), callStack);
   }

    // Gets the value of the cell at loc
    private double locToDouble(Location loc, Deque<RealCell> callStack) throws IllegalArgumentException {
        Cell curCell = sheetRef.getCell(loc);

        // Formula refers to something that is not a RealCell
        if (!(curCell instanceof RealCell)) {
            throw new IllegalArgumentException();
        }

        // curCell is a RealCell
        return ((RealCell) curCell).getDoubleValue(callStack);
    }

    // Returns true if token is an operator
    private static boolean isOperator(String token) {
        boolean isOperator = false;

        if (token.equals("+")) {
            isOperator = true;
        } else if (token.equals("-")) {
            isOperator = true;
        } else if (token.equals("*")) {
            isOperator = true;
        } else if (token.equals("/")) {
            isOperator = true;
        } else if (token.equals("(")) {
            isOperator = true;
        } else if (token.equals(")")) {
            isOperator = true;
        } else if (token.equalsIgnoreCase("sum")) {
            isOperator = true;
        } else if (token.equalsIgnoreCase("avg")) {
            isOperator = true;
        }
        return isOperator;
 }

    @Override
    public String fullCellText() {
        return "( " + formula + " )";
    }
}
