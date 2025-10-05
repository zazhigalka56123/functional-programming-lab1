package lab1;

public class Euler19 {

    public static boolean isLeap(int year) {
        return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    }

    public static int solve(int startYear, int endYear) {
        int sundays = 0;
        int dayOfWeek = 1;

        for (int year = startYear; year <= endYear; year++) {
            for (int month = 1; month <= 12; month++) {
                if (dayOfWeek == 6) {
                    sundays++;
                }

                int days = 31;
                if (month == 4 || month == 6 || month == 9 || month == 11) {
                    days = 30;
                } else if (month == 2) {
                    days = isLeap(year) ? 29 : 28;
                }
                dayOfWeek = (dayOfWeek + days) % 7;
            }
        }
        return sundays;
    }
}