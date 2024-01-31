---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה בקובץ טקסט היא פעולה בה אנחנו שומרים נתונים או מידע בקובץ מתוך התוכנית שלנו. תכניתנים עושים זאת כדי לשמור נתונים לשימוש מאוחר יותר, לאחסון לוגים ולצרכי דיבאג.

## איך לעשות:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class TextFileWriter {
    public static void main(String[] args) {
        String content = "שלום, עולם!";
        String path = "output.txt";
        
        // גישה עם BufferedWriter
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(path))) {
            writer.write(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        // גישה עם Files
        try {
            Files.write(Paths.get(path), content.getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

הפלט בשני המקרים יהיה קובץ בשם `output.txt` עם הטקסט "שלום, עולם!".

## מבט מעמיק:
היכולת לכתוב לקבצי טקסט קיימת מראשית ימי התכנות והיא אמצעי חשוב לשמירת מידע באופן פשוט וגישה. בנוסף לגישות שהוצגו, יש גם אפשרות כתיבה באמצעות `PrintWriter`, `FileOutputStream` ועוד. חשוב לטפל בשגיאות עם try-catch כדי לא לגרום לבעיות בריצה או אובדן מידע, והשימוש בטכניקת try-with-resources מבטיח שהמשאבים יסגרו תקין.

## ראה גם:

- [BufferedWriter Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html)
- [Files.write Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#write(java.nio.file.Path,byte[],java.nio.file.OpenOption...))
- [Handling IO exceptions in Java](https://docs.oracle.com/javase/tutorial/essential/exceptions/handling.html)
