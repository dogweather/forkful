---
title:                "Java: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

למה שמישהו יתעסק בכתיבת קובץ טקסט? הכתיבה של קבצי טקסט היא חלק חשוב מתהליך התכנות ונחשבת ליסודית בכמעט כל שפת תכנות. היא מאפשרת לנו ליצור קבצים שמכילים מידע שניתן לקרוא ולהבין על ידי מחשב, והיא תחבירה קלה לשפה המוכרת והנוחה למתכנתים כדי לכתוב תוכניות מחשב.

## כיצד ל

כדי לכתוב קובץ טקסט בשפת ג'אווה, נוכל להשתמש בפעולות ממערך בסיסי כדי לכתוב מחרוזות טקסט לתוך הקובץ. הנה דוגמא של קוד הגנרציה של קובץ טקסט שיכתוב את הטקסט "שלום עולם":

```Java
import java.io.FileWriter;
import java.io.IOException;

public class CreateTextFile {
   public static void main(String[] args) {
       try {
           // אתחול אובייקט FileWriter כדי לכתוב לקובץ שלך
           FileWriter myFile = new FileWriter("mytextfile.txt");

           // כתיבת המחרוזת "שלום עולם" לקובץ
           myFile.write("שלום עולם");

           // סגירת הקובץ
           myFile.close();
       } catch (IOException e) {
           System.out.println("שגיאה בכתיבת הקובץ");
           e.printStackTrace();
       }
   }
}
```

לאחר הרצת הקוד, תוכל לראות שנוצר קובץ חדש בשם "mytextfile.txt" עם התוכן "שלום עולם".

## יצירה עמוקה

כמו שאנו ראינו בדוגמא הנ"ל, לכתוב קובץ טקסט בשפת ג'אווה זה פשוט. אולם, ישנן טכניקות מתקדמות יותר ליצירת קבצי טקסט וכתיבתם. לדוגמא, ניתן להשתמש בבנאי PrintWriter עבור פעולת הכתיבה, ולהוסיף מאפיינים נוספים כמו ניתוח ולכתוב ישירות לקובץ במקום לצור אותו שכלי גרפי לעריכת ה