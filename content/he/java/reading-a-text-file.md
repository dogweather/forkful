---
title:    "Java: קריאת קובץ טקסט"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

נקרא קובץ טקסט הוא משימה נפוצה וחשובה בתחום התכנות. זה יכול לסייע בפיתוח תוכניות שמשתמשות בנתונים מקוריים על ידי קריאת קובץ טקסט. כמו כן, זה עשוי לשמש ככלי לקריאת מידע מקבצי טקסט שמבטיחים חומרה עצמית של נתוני משעור שכל

## כיצד

ראשית, ניצור קובץ טקסט פשוט בשם "input.txt". בתוכו נרשום כמה שורות טקסט כדוגמא עבור הקוד שנכתוב:

```
Hello World!
This is a sample text file.
We will read this file using Java.
```

כעת ניצור בתוכו תוכנית בשפת ג'אווה כדי לקרוא את הקובץ הנ"ל ולהדפיס את הטקסט שבתוכו:

```
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ReadFileExample {

    public static void main(String[] args) {
        try {
            // Create a File object for the text file
            File file = new File("input.txt");
            // Create a Scanner object to read the file
            Scanner scanner = new Scanner(file);
            // Use a while loop to read each line in the file
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                // Print the line to the console
                System.out.println(line);
            }
            // Close the Scanner object to release resources
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found.");
        }
    }
}
```

הפלט של תוכנית זו יהיה:

```
Hello World!
This is a sample text file.
We will read this file using Java.
```

כפי שניתן לראות, הקוד משתמש בטכניקות קריאת קבצים בשפת ג'אווה כדי לקרוא כל שורה בקובץ ולהדפיס אותה.

## מעמקים נמשך

קריאת קובץ טקסט היא רק פעולה בסיסית שבעזרתה ניתן לקרוא מידע מקובץ כלשהו. בנוסף, ניתן להשתמש בטכניקות נוספות כגון כתיבת נתונים לקובץ טקסט, יצירת קובץ טקסט ועוד. כמו כן, ניתן להשתמש במחלקות נוספות בג'אווה כדי לקרוא קבצים