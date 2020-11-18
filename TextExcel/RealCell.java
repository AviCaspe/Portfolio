package TextExcel;

import java.util.Deque;

// A RealCell is a cell that holds a Real Number
public abstract class RealCell extends ACell {

    @Override
    public abstract String fullCellText();

    @Override
    public String abbreviatedCellText() {
        try {
            return super.formatCellText(Double.toString(this.getDoubleValue()));
        } catch (IllegalArgumentException e) {
            return super.formatCellText("#ERROR");
        }
    }

    public abstract double getDoubleValue();

    protected abstract double getDoubleValue(Deque<RealCell> callStack);

    @Override
    public int compareTo(Cell other) {
        // Compared to a RealCell
        if (other instanceof RealCell) {
            double thisValue = this.getDoubleValue();
            double otherValue = ((RealCell) other).getDoubleValue();
            if (thisValue < otherValue) {
                return -1;
            } else if (thisValue == otherValue) {
                return 0;
            } else if (thisValue > otherValue) {
                return 1;
            }
        }

        // A RealCell is greater than everything else
        return 1;
    }

}
