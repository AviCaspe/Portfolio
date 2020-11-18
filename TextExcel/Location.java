package TextExcel;

public class Location {
    // These values are zero based
    // So A1 is row 0 and column 0
    private int row;
    private int col;

    // This constructor assumes that cellName has valid cell syntax
    // B3, A12, and D9 are all valid examples of cell syntax
    // This constructor also doesn't care about capitalization of the column letter
    public Location(String cellName) {
        // Since the first row of the spreadsheet is 1
        // And the first row of an array is 0
        // We need to subtract 1 to have a zero based index
        this.row = Integer.parseInt(cellName.substring(1)) - 1;

        // This gets the Cell's column without caring about the letter's case
        this.col = Character.toUpperCase(cellName.charAt(0)) - 'A';
    }

    // This constructor assumes that row and column are zero based
    public Location(int row, int col) {
        this.row = row;
        this.col = col;
    }

    public int getRow() {
        return this.row;
    }

    public int getCol() {
        return this.col;
    }

    @Override
    public String toString() {
        return "Row: " + this.row + " Col: " + this.col;
    }
}
