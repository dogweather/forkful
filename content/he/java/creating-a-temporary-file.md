---
title:                "Java: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע

הפוסט הזה מיועד למתכנתים בשפת ג'אווה העוסקים ביצירת קבצים זמניים. כאן יש לכם את הסיבות הגורמות ליצירת קובץ זמני ותיאור כיצד לעשות זאת בקלות ובפשטות.

## איך לעשות זאת

השימוש בקבצים זמניים יכול להיות מאוד שימושי במגוון מצבים. בקבצים זמניים אפשר לשמור נתונים זמניים שלא נשמרים לצמיתות ולשנותם במהירות, או להתאים את הקוד הולם שלכם בכדי לטפל באירועים כמו כן. תמצאו כאן קוד דוגמא ופלט ואת הדרך ליצירת קובץ זמני.

```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {

    public static void main(String[] args) {
        try {
            // יצירת קובץ זמני עם פקודה שתפלאן מחדש את הקובץ תמיד. כדי להתאים את התבנית
            // בפקד ל-File.createTempFile, כדי ליצור קובץ זמני עם הסיומת הזאת.
            File temp = File.createTempFile("tempfile", ".txt");

            // נדמיין כאן כדי לייצג את הנתונים שאנו רוצים לשמור בקובץ הזמני.
            String data = "מידע זה מיוצג בקובץ זמני.";

            // עכשיו נכתוב את הטקסט לקובץ הזמני.
            FileWriter writer = new FileWriter(temp);
            writer.write(data);
            writer.close();

            // כאן נדפיס את הנתונים מהקובץ הזמני שיצרנו. מיוצג עם פקודת println בכדי להדפיס
            // את התוכן לכונסומט.

            // כיוון שאנו משתמשים בקובץ זמני, התכנית תימחק באופן אוטומטי אחרי הביצוע.
            FileReader reader = new FileReader(temp);
            BufferedReader br = new BufferedReader(reader);
            String line;
            while ((line = br.readLine()) != null) {
                System.out.println(line);
            }
            reader.close();

            // בסיום נמחק את הקובץ הזמני.
            if (temp.delete()) {
                System.out.println("קובץ