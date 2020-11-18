package TextExcel;

import java.util.Scanner;

public class TextExcel {
    public static void main(String[] args) {
        Spreadsheet sheet = new Spreadsheet();
        Scanner in = new Scanner(System.in);

        // Main processing loop
        System.out.println(sheet.getGridText());

        String command = in.nextLine();
        while (!command.trim().equalsIgnoreCase("quit")) {
            System.out.println(sheet.processCommand(command));
            command = in.nextLine();
        }
    }
}
