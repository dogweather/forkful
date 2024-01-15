---
title:                "קריאת קובץ טקסט"
html_title:           "Java: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

אנשים יכולים להיעניין לקרוא קובץ טקסט מתוך כוונה להתעמק בתכנות בשפת ג'אווה. זה יכול לסייע להם להבין עוד יותר על השפה וכיצד להשתמש בה בצורה יעילה ויצירתית.

## כיצד לעשות זאת

כדי לקרוא קובץ טקסט בשפת ג'אווה, יש לעקוב אחר השלבים הבאים:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TextFileReader {
    public static void main(String[] args) {
        
        // הגדרת משתנה לקובץ הטקסט
        File file = new File("textfile.txt");
        
        try {
            // יצירת אובייקט לקריאת הקובץ
            Scanner scanner = new Scanner(file);
            
            // קריאת הקובץ עד שהגענו לסוףו
            while (scanner.hasNextLine()) {
                // הדפסת השורה הנוכחית
                System.out.println(scanner.nextLine());
            }
            
            // סגירת הסורק
            scanner.close();
        } catch (FileNotFoundException e) {
            // איפוס השגיאה אם הקובץ לא נמצא
            e.printStackTrace();
        }
    }
}
```

פלט עבור הקוד הבא יכיל את כל הנתונים המצויים בקובץ הטקסט:

```text
שורה ראשונה
שורה שנייה
שורה שלישית
```

## טיול עמוק

כדי לקרוא קובץ טקסט עם יותר מתייחסים לפרטיות כמו גודל הקובץ, הנוכחות של סיווגי התווים ועוד. בנוסף, ניתן גם להשתמש בפונקציות נוספות כגון `hasNext()` ו- `next()`, שמאפשרות לקרוא את הקובץ באופן מתקדם יותר. ניתן למצוא מידע נוסף על השימוש בקובץ טקסט בשפת ג'אווה בכתבות נוספות כמו "כיצד לכתוב לקובץ טקסט בשפת ג'אווה" ו"שימוש בקובץ קיים בשפת ג'אווה".

## ראו גם

- [כיצד לכתוב לקובץ טק