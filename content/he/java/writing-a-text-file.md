---
title:    "Java: כתיבת קובץ טקסט"
keywords: ["Java"]
---

{{< edit_this_page >}}

# מדוע:
אנשים יכולים לכתוב קובץ טקסט מסקרן ביותר. ייתכן שתרצו לשמור תוכן טקסטואלי כמו דוחות או גיבויי נתונים בצורה נוחה ונגישה. בנוסף, קידוד ותחזוקת קבצי טקסט הם נהדרים בשביל שתהיה בטוח שאת הנתונים שלך ייהיה קשה לבזבז או להחליף בטעות.
סיכום: במאמר זה נלמד לכתוב ולנהל קבצי טקסט בשפת ג'אווה.

# איך לעשות:
```Java
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException; 

public class Main {
    public static void main(String[] args) throws IOException {
        // כתיבת מחרוזת לקובץ טקסט חדש
        String text = "מה קורה, עולם?";
        // יצירת אובייקט של FileWriter וקובץ חדש
        FileWriter fileWriter = new FileWriter("newFile.txt");
        // יצירת אובייקט של BufferedWriter שיגדיר לכתוב לקובץ שנצר
        BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
        // כתיבת המחרוזת לקובץ וסגירת הקובץ
        bufferedWriter.write(text);
        bufferedWriter.close();
        // הדפסת הפלט "קובץ נוסף בהצלחה!"
        System.out.println("קובץ נוסף בהצלחה!");
    }
}
```
השורות הראשונות בקוד מייבאות מספר חבילות של ג'אווה השמות של סופר-חבילות שמייצגות את הפונקציונאליות הנחוצה. קובצי טקסט נוצרים עם שימוש באובייקט FileWriter ו-BufferedWriter מאפשר לך לכתוב קובץ טקסט בצורה יציבה ומהירה.

הפתרון המוצע מכיל שני אובייקטים חדשים: FileWriter ו-BufferedWriter. בשורה השנייה יוצרים אובייקט של FileWriter לכתיבת קובץ חדש. בשורה השלישית, יוצרים אובייקט של BufferedWriter אשר מוׅן לכתוב לקובץ שנוצר על ידי fileWriter. מתבצע כתיבת המחר