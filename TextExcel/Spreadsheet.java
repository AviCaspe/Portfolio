package TextExcel;

import java.util.*;
import java.util.stream.Collectors;

public class Spreadsheet {
    private int rows;
    private int cols;
    private Cell[][] spreadsheet;

    public Spreadsheet() {
        this.rows = 20;
        this.cols = 12;
        this.spreadsheet = new Cell[rows][cols];
        History.getHistory().init();
        this.clear();
    }

    public String processCommand(String command) {
        // Variables for logging history
        String output;
        String originalCommand = command;
        boolean logCommand = true;

        command = command.trim();
        // Cell Inspection
        if (command.matches("[A-Za-z][0-9][0-9]?")){
            Cell cell = this.getCell(new Location(command));
            output = cell.fullCellText();
        // Assignment
        } else if (command.contains("=")) {
            // Seperate the cell and the contents
            String[] setCommand = command.split("\\s*=\\s*", 2);
            this.set(setCommand);
            output = this.getGridText();
        } else if (command.toLowerCase().contains("history")) {
            // Don't log this command
            logCommand = false;
            // Grab the arguments to the history command
            output = this.processHistoryCommand(command.split("\\s+", 2)[1].split("\\s+"));
        // Clear all
        } else if (command.equalsIgnoreCase("clear")) {
            this.clear();
            output = this.getGridText();
        // Clear a single cell
        } else if (command.toLowerCase().contains("clear")) {
            this.clear(new Location(command.substring(6)));
            output = this.getGridText();
        } else if (command.toLowerCase().contains("sorta")) {
            this.sort(command.split("\\s+")[1], 1);
            output = this.getGridText();
        } else if (command.toLowerCase().contains("sortd")) {
            this.sort(command.split("\\s+")[1], -1);
            output = this.getGridText();
        } else {
            output = "";
        }

        if (logCommand) {
            History.getHistory().add(originalCommand);
        }
        return output;
    }

    // Sets the value of a cell
    // Also creates a new cell object of the correct type
    // command[0] = location
    // command[1] = value
    private void set(String[] command) {
        Location loc = new Location(command[0]);
        String value = command[1];
        // TextCell
        if (value.contains("\"")) {
            // Substring extracts the String's text
            String contents = value.substring(1, value.length() - 1);
            this.set(loc, new TextCell(contents));
        // PercentCell
        } else if (value.contains("%")) {
            double percentValue = Double.parseDouble(value.substring(0, value.length() - 1));
            this.set(loc, new PercentCell(percentValue));
        // FormulaCell
        } else if (value.contains("(")) {
            // Extract just the formula from the value string
            String formula = value.substring(value.indexOf("(") + 1, value.lastIndexOf(")")).trim();
            this.set(loc, new FormulaCell(formula, this));
        // ValueCell
        } else {
            this.set(loc, new ValueCell(Double.parseDouble(value)));
        }
    }

    // loc now contains cell
    private void set(Location loc, Cell cell)  {
        this.spreadsheet[loc.getRow()][loc.getCol()] = cell;
    }

    // Sets every cell in the spreadsheet to an EmptyCell
    private void clear() {
        for (int row = 0; row < spreadsheet.length; row++) {
            for (int col = 0; col < spreadsheet[row].length; col++) {
                spreadsheet[row][col] = new EmptyCell();
            }
        }
    }

    // Sets just one cell to an EmptyCell
    private void clear(Location loc) {
        spreadsheet[loc.getRow()][loc.getCol()] = new EmptyCell();
    }

    // Processes a history command and sends it to the history object
    public String processHistoryCommand(String[] command) {
        // Default output
        String output = "";
        // Current History
        History hist = History.getHistory();

        // Process the command
        if (command[0].equalsIgnoreCase("stop")) {
            hist.stop();
        } else if (command[0].equalsIgnoreCase("display")) {
            output = hist.getContents();
        } else if (command[0].toLowerCase().equals("start")) {
            hist.start(Integer.parseInt(command[1]));
        } else if (command[0].toLowerCase().equals("clear")) {
            hist.clear(Integer.parseInt(command[1]));
        }
        return output;
    }

    // Parses the range, gets the cells,
    // calls the sort algorithm,
    // and then reassigns the cells
    private void sort(String range, int factor) {
        // Get all the cells in the region
        String[] endPoints = range.split("-");
        Location ULCorner = new Location(endPoints[0]);
        Location DRCorner = new Location(endPoints[1]);

        List<Location> locs = Spreadsheet.getLocsInRegion(ULCorner, DRCorner);
        List<ACell> cells = Spreadsheet.getLocsInRegion(ULCorner, DRCorner)
                                       .stream()
                                       .map(x -> (ACell) this.getCell(x))
                                       .collect(Collectors.toList());
        // Sort the cells
        Spreadsheet.quicksort(cells, 0, cells.size() - 1, factor);

        // Reassign the cells to their new locations
        for (int i = 0; i < locs.size(); i++) {
            this.set(locs.get(i), cells.get(i));
        }
    }

    // Sorts all elements of the cells list that lie within the range startIndex to endIndex inclusive
    // Factor determines if we are sorting asceding or desceding
    private static void quicksort(List<ACell> cells, int startIndex, int endIndex, int factor) {
        // Partitioning the array

        // Keep track of which elements we've examined
        int compIndex = startIndex;
        // Use the last element as the pivot
        int pivotIndex = endIndex;

        while (compIndex < pivotIndex) {
            // compCell comes before the pivot
            // so no movement is needed
            // compIndex is updated so we get a new element to compare to
            if (factor * cells.get(compIndex).compareTo(cells.get(pivotIndex)) < 0) {
                compIndex++;

            // compCell goes after the pivot
            // Moving compCell to the end of the list
            // Moves the pivot forward 1 element,
            // and brings a new element into the compIndex
            } else {
                moveToEndOfSublist(cells, compIndex, endIndex);
                pivotIndex--;
            }
        }

        // Sort the left if needed
        if (pivotIndex - startIndex > 1) {
            quicksort(cells, startIndex, pivotIndex - 1, factor);
        }
        // Sort the right if needed
        if (endIndex - pivotIndex > 1) {
            quicksort(cells, pivotIndex + 1, endIndex, factor);
        }
    }

    // Moves the item at elementIndex to the spot following endOfSublist
    private static void moveToEndOfSublist(List<ACell> cells, int elementIndex, int endOfSublist) {
        // Adding to the end of the list
        if (endOfSublist == cells.size() - 1) {
            cells.add(cells.remove(elementIndex));

        // Adding to the middle of the array
        } else {
            cells.add(endOfSublist, cells.remove(elementIndex));
        }
    }

    // Using a StringBuilder here saves memory
    public String getGridText() {
        StringBuilder grid = new StringBuilder();

        // Header
        grid.append("   |");
        for (int i = 0; i < this.cols; i++) {
            String colLetter = Character.toString((char) ('A' + i));
            grid.append(pad(colLetter, 10) + "|");
        }

        // Print the rows
        for (int rows = 0; rows < this.rows; rows++) {
            // Get the row number properly spaced
            grid.append("\n" + pad(Integer.toString(rows + 1), 3) + "|");
            // Append the row content
            for (int cols = 0; cols < this.cols; cols++) {
                Cell curCell = this.getCell(new Location(rows, cols));
                grid.append(curCell.abbreviatedCellText() + "|");
            }
        }
        grid.append("\n");

        return grid.toString();
    }

    // Adds spaces to the end of a string, so that the string has length chars
    private static String pad(String s, int length) {
        for (int i = s.length(); i < length; i++) {
            s += " ";
        }
        return s;
    }

    // Returns a list of all the locations in a given region
    public static List<Location> getLocsInRegion(Location ULCorner, Location DRCorner) {
        List<Location> locs = new ArrayList<Location>(Spreadsheet.getRegionSize(ULCorner, DRCorner));

        // Iterate through the region
        for (int row = ULCorner.getRow(); row <= DRCorner.getRow(); row++) {
            for (int col = ULCorner.getCol(); col <= DRCorner.getCol(); col++) {
                locs.add(new Location(row, col));
            }
        }

        return locs;
    }

    public static int getRegionSize(Location ULCorner, Location DRCorner) {
        return ((DRCorner.getRow() - ULCorner.getRow() + 1) * (DRCorner.getCol() - ULCorner.getCol() + 1));
    }

    public Cell getCell(Location loc) {
        return spreadsheet[loc.getRow()][loc.getCol()];
    }
}
