package TextExcel;

public class PercentCell extends ValueCell {
    // A Percent is a value like 50.4
    private double percentValue;

    // value should be a decimal like 1.23
    public PercentCell(double percentValue) {
        super(percentValue / 100);
        this.percentValue = percentValue;
    }

    @Override
    public String fullCellText() {
        return percentValue + "%";
    }

    @Override
    public String abbreviatedCellText() {
        return super.formatCellText(((int) percentValue) + "%");
    }
}
