---
title:                "יצירת קובץ זמני"
html_title:           "Java: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יש סיבות מגוונות להשתמש בקבצי זמניים בתוכניות ג'אווה. הקבצים הזמניים שמייצרים יכולים לשמש בתהליך פיתוח כדי לבדוק תכנית, לאחסן נתונים זמניים או לכתוב תוכניות עם פעם אחת תצוגה של קבצי טקסט כחובה.

## איך לעשות זאת

יצירת קובץ זמני בתוכנית ג'אווה היא פשוטה מאוד, תוכלו לעשות זאת באמצעות הפעולות הבאות:

```Java
// ייבוא המחלקה הנדרשת
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {
    public static void main(String[] args) {

        // נגדיר את המשתנה הזמני
        File tempFile = null;
 
        try {
            // ניצור את הקובץ הזמני
            tempFile = File.createTempFile("tempFile", ".txt");
 
            // הדפסת נתונים לקובץ הזמני
            System.out.println("קובץ זמני נוצר בהצלחה בכתובת: " + tempFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### פלט דוגמה:

קובץ זמני נוצר בהצלחה בכתובת: C: \ Users \ [שם משתמש] \ AppData \ Local \ Temp \ tempFile5085285971402607141.txt

## חפירה

כאשר אתם מייצרים קובץ זמני בתוכנית ג'אווה, ישנן מספר התיווכויות שעליכם להיות מודעים אליהן:

- כשעושים שימוש בפונקציה `createTempFile`, אתם צריכים לספק שמות לקובץ הזמני וסיומת מתאימה.
- קבצים זמניים נמחקים אוטומטית כאשר התוכנית נסגרת.
- ניתן לקרוא, לכתוב ולהוסיף נתונים לקובץ הזמני כמו שמתבקש.
- ניתן להשתמש בקבצי זמניים גם בתוך פקודת `try-catch`, כדי לטפל בשגיאות.

## לראות גם

למידע נוסף על קבצים זמניים בג'אווה, נית