---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

יצירת קובץ זמני הוא תהליך בו מנוצלת מקום בדיסק כל עוד האפליקציה פעילה. מתכנתים עשויים לבחור ליצור קובץ זמני למגוון סיבות, אם זה לאחסון נתונים מחיצוני מאפליקציה שאינם נחוצים לאחר מכן, או להשתמש בותיק גיבוי שנמחק באופן אוטומטי.

## איך לבצע:

כדי ליצור קובץ זמני ב- Java, אנו משתמשים בפעולה `createTempFile()` של מחלקת `java.nio.file.Files`:

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileCreation {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile("myTempFile", ".txt");
            System.out.println("Temporary file path: " + tempFile.toString());

        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
```
הפלט של חלק הקוד הזה יהיה:

```text
Temporary file path: /tmp/myTempFile1234567890.txt
```

## צלילה עמוקה

יצירת קובצים זמניים הייתה חלק מ- Java מאז הגרסה 1.2, כדי לתמוך ביצירת קבצים באופן דינמי שאינם דורשים אחסון לאורך טווח. נוסף אליו, שיטות אחרות של יצירת קובצים זמניים כוללות השתמש במחלקת `java.io.File` (מתוך הגרסה הראשונה של Java), או באמצעות מנהלים למידע חיצוניים מהמערכת שלך, כמו Apache Commons IO.

במהלך יצירת קובץ זמני, Java מפעילה מספר פעולות: היא מחפשת מיקום במערכת הקבצים שבו יש מקום זמין, פותחת מקום בדיסק, ויוצרת הפניה לקובץ באופן שמאפשר לאפליקציה לגשת לו. ברגע שהאפליקציה מבצעת יציאה מלאה, המערכת אוטומטית מוחקת את כל הקבצים הזמניים שנוצרו.

## ראה גם

- Java API Documentation: [Path](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
- Java API Documentation: [Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- Apache Commons IO: [File Utilities](https://commons.apache.org/proper/commons-io/javadocs/api-release/org/apache/commons/io/FileUtils.html)