---
date: 2024-01-20 17:41:53.504122-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05D4\u05D9\u05D0 \u05D3\u05E8\
  \u05DA \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05E9\u05D0\u05DE\
  \u05D5\u05E8 \u05DC\u05D4\u05D9\u05DE\u05D7\u05E7 \u05DC\u05D0\u05D7\u05E8 \u05E9\
  \u05EA\u05DB\u05DC\u05D9\u05EA\u05D5 \u05D1\u05EA\u05DB\u05E0\u05D9\u05EA \u05E0\
  \u05D2\u05DE\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05E9\u05D4\u05DD \u05E6\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D6\u05DE\u05E0\u05D9 \u05D1\u05DE\
  \u05D4\u05DC\u05DA \u05D4\u05E8\u05E6\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.164100-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05D4\u05D9\u05D0 \u05D3\u05E8\u05DA\
  \ \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05E9\u05D0\u05DE\u05D5\
  \u05E8 \u05DC\u05D4\u05D9\u05DE\u05D7\u05E7 \u05DC\u05D0\u05D7\u05E8 \u05E9\u05EA\
  \u05DB\u05DC\u05D9\u05EA\u05D5 \u05D1\u05EA\u05DB\u05E0\u05D9\u05EA \u05E0\u05D2\
  \u05DE\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05E9\u05D4\u05DD \u05E6\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D6\u05DE\u05E0\u05D9 \u05D1\u05DE\u05D4\
  \u05DC\u05DA \u05D4\u05E8\u05E6\u05EA\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני בג'אווה היא דרך ליצור קובץ שאמור להימחק לאחר שתכליתו בתכנית נגמרת. תכניתאים עושים זאת כשהם צריכים לשמור נתונים באופן זמני במהלך הרצת התכנית, ללא צורך להשאיר אותם אחר כך.

## איך לעשות:

כדי ליצור קובץ זמני בג'אווה, נשתמש במחלקת `Files`:

```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TemporaryFileExample {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile(null, ".tmp");
            System.out.println("Temporary file created at: " + tempFile);

            // לדוגמה, כאן אנחנו יכולים לכתוב לקובץ, לקרוא ממנו, וכו'...

            // בסוף, אנחנו יכולים למחוק את הקובץ אם נרצה
            Files.delete(tempFile); 
            System.out.println("Temporary file deleted: " + tempFile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

התוצאה:
```
Temporary file created at: /tmp/1234567890.tmp
Temporary file deleted: /tmp/1234567890.tmp
```

## עיון מעמיק:

המחלקה `java.io.File` הייתה הדרך הראשונית ליצור קבצים זמניים, אבל מאז ג'אווה 7, המחלקה `java.nio.file.Files` הפכה לאופציה מועדפת בשל ממשקה הנוח והחזק יותר. המתודה `createTempFile` יכולה לקבל פרמטרים לשמות קובץ (prefix ו-suffix) ונתיב התיקייה עבור הקובץ הזמני, אם יש צורך בשליטה רבה יותר.

הקבצים הזמניים יכולים לשמש למקרים כמו כאשר האפליקציה צריכה לטפל בנתונים שהורדו מהאינטרנט, או במצבים בהם יש צורך לשמור מצב ביניים במערכת קבצים. השימוש בכך הוא גם בעל יתרון ביצועים וביטחון, מאחר ומזער את הסבירות להתנגשויות בקבצים ומנקה אחרי עצמו אם זה נעשה נכון.

## ראה גם:

- תיעוד רשמי של מחלקת `Files`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html
- מדריך לניהול קבצים ותיקיות בג'אווה: https://docs.oracle.com/javase/tutorial/essential/io/
- תיעוד המחלקה `Path`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Path.html
