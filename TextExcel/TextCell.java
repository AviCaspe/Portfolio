package TextExcel;

public class TextCell extends ACell {
    private String text;

    public TextCell(String text) {
        this.text = text;
    }

    @Override
    public String fullCellText() {
        return "\"" + this.text + "\"";
    }

    @Override
    public String abbreviatedCellText() {
        return super.formatCellText(this.text);
    }

    @Override
    public int compareTo(Cell other) {
        if (other instanceof EmptyCell) {
            return 1;
        }
        if (other instanceof RealCell) {
            return -1;
        }
        return this.text.compareTo(((TextCell) other).getText());
    }

    public String getText() {
        return this.text;
    }
}
