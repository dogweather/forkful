---
title:                "Java: יצירת קובץ זמני"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא תהליך חשוב ונפוץ בתכנות בג'אווה. קבצים זמניים משמשים למגוון רחב של מטרות, כגון כתיבת נתונים מתוך התכנית, קריאת נתונים לאחר מכן, יצירת זיכרונות מזמינים, ועוד. ברגע שתלמדו איך ליצור קובץ זמני, תפתחו עוד דלת ליצור יישומים מתקדמים יותר בג'אווה.

## איך ליצור קובץ זמני בג'אווה

כדי ליצור קובץ זמני בג'אווה, ניתן להשתמש במחלקה המובנית "File" ולהשתמש בשיטה "createTempFile". להלן דוגמא של קוד ליצירת קובץ זמני בג'אווה והדפסת הנתונים בתוכו:

```Java
// ייבוא חבילת המחלקה File
import java.io.File;
// ייבוא החבילה המכילה את השכבת הדפדפן
import java.nio.file.FileSystem;

// בניית המחלקה עם שם ראשי "CreateTempFile"
public class CreateTempFile{
    // נקבע את הראשיות של המחלקה
    public static void main(String[] args) {
        try{
            // יצירת קובץ זמני בהתאם לשם וסיומת שכבר הוגדרו
            File tempFile = File.createTempFile("myfile", ".txt");
            // כתיבת המחרוזת המוגדרת לתוך הקובץ הזמני
            FileSystem.write("תוכן הקובץ הזמני");
            // הדפסת הנתונים מתוך הקובץ
            System.out.println("נתונים מתוך הקובץ הזמני: " + tempFile.getContent());
        }
        catch(Exception e){
            System.out.println("התרחשה שגיאה ביצירת הקובץ");
        }
    }
}
```

הפלט המתוחזק יהיה:

```
נתונים מתוך הקובץ הזמני: תוכן הקובץ הזמני
```

## נכנסים לעומק

על מנת להבין את תהליך יצירת קובץ זמני בג'אווה, כדאי להבין כי הקבצים הזמניים נוצרים בכתובת ה־"tmp"