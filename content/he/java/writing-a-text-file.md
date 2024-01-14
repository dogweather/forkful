---
title:                "Java: כתיבת קובץ טקסט"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

##למה:

כתיבת קובץ טקסט היא דבר שמוכר לרובנו - זהו הפעולה הגורפת במיוחד כאשר אנחנו משתמשים במחשבים ובתכנות. אנו פתוחים קבצי טקסט כדי לכתוב דואר אלקטרוני, לכתוב קובץ ממירוץ, לנהל רשימת מטלות, ועוד. כתיבת קובץ טקסט היא כלי נפלא המאפשר לנו ליצור, לשמור ולערוך נתונים בפורמט טקסט פשוט ונגיש. בכתבה זו נלמד כיצד ליצור קובץ טקסט עם קוד Java פשוט.

##איך לעשות זאת:

על מנת ליצור קובץ טקסט עם קוד Java, ניצור פרויקט חדש בסביבת פיתוח כלשהי ונשבץ אליו את הקוד הבא:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {

        //כתיבת נתונים לקובץ טקסט
        try {
            FileWriter writer = new FileWriter("myTextFile.txt");
            BufferedWriter buffer = new BufferedWriter(writer);
            buffer.write("זוהי הכתבה שלנו על כתיבת קובץ טקסט עם קוד Java.");
            buffer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

הקוד הנ"ל מייצר קובץ טקסט חדש בשם "myTextFile.txt" ומכתיב לתוכו את המחרוזת "זוהי הכתבה שלנו על כתיבת קובץ טקסט עם קוד Java." כדי להראות תוצאה נוסיפה, ניתן להדפיס את תוכן הקובץ בעזרת הקוד הבא:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class TextFileReader {
    public static void main(String[] args) {

        //קריאת נתונים מקובץ טקסט
        try {
            FileReader reader = new FileReader("myTextFile.txt");
            BufferedReader buffer = new BufferedReader(reader);
            String line = buffer.readLine();
            System.out.println(line);
            buffer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

בקוד הנ"ל, אנו קוראים את הקובץ שנוצר בקוד הראשון ומדפיסים את תוכןו. התוצאה שתודפס