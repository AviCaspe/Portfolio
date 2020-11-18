package TextExcel;

import java.util.Deque;

public class ValueCell extends RealCell {
    private double doubleValue;

    public ValueCell(double doubleValue) {
        this.doubleValue = doubleValue;
    }

    @Override
    public String fullCellText() {
        return Double.toString(this.doubleValue);
    }

    @Override
    public double getDoubleValue() {
        return this.doubleValue;
    }

    @Override
    protected double getDoubleValue(Deque<RealCell> callStack) {
        return this.doubleValue;
    }
}
