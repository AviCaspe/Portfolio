package TextExcel;

public class EmptyCell extends ACell {
    @Override
    public String fullCellText() {
        return "";
    }

    @Override
    public String abbreviatedCellText() {
        return super.formatCellText("");
    }

    @Override
    public int compareTo(Cell other) {
        // Compare to an EmptyCell
        if (other instanceof EmptyCell) {
            return 0;
        }

        // An EmptyCell is less than everything else
        return -1;
    }
}
