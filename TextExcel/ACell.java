package TextExcel;

// This is an abstract cell class, providing an implementation formatCellText
// formatCellText is called on each cell, when building the string that is the spreadsheet
public abstract class ACell implements Cell, Comparable<Cell> {
    @Override
    public abstract String fullCellText();

    @Override
    public abstract String abbreviatedCellText();

    public abstract int compareTo(Cell other);

    // Format a cell so that it's exactly 10 characters long
    // Either append spaces, or truncate as needed.
    protected static String formatCellText(String s) {
        if (s.length() < 10) {
            for (int i = s.length(); i < 10; i++) {
                s += " ";
            }
            return s;
        } else {
            return s.substring(0, 10);
        }
    }

    @Override
    public String toString() {
        return this.fullCellText();
    }
}
