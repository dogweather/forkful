---
title:    "Java: בדיקת קיום תיקייה במחשב"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

למה כדאי לבדוק אם ספרייה קיימת: כאשר אנו מפתחים יישומי קוד מודולריים, חשוב לוודא שהספריות שאותן היישומים משתמשים בהן קיימות וזמינות. כמו כן, בדיקת קיות ספרייה גם תעזור למנוע תקלות והתעקשויות במהלך הריצה של האפליקציה.

## איך לבדוק אם ספרייה קיימת:

ב Java ישנם מספר דרכים לבדוק אם ספרייה קיימת במערכת הקבצים. אחת הדרכים היא להשתמש במחלקת `java.io.File` ובשיטת `exists()`. 

```Java
File directory = new File("myDirectory");
if (directory.exists()) {
    System.out.println("הספרייה קיימת.");
} else {
    System.out.println("הספרייה אינה קיימת.");
}
```

בנוסף, אפשר גם להשתמש בשיטת `isDirectory()` כדי לוודא שמדובר בספרייה ולא בקובץ רגיל.

```Java
File directory = new File("myDirectory");
if (directory.isDirectory()) {
    System.out.println("זוהי ספרייה.");
} else {
    System.out.println("אינה ספרייה.");
}
```

## Deep Dive

התמיכה בבדיקת קיות ספריות הוא חלק חשוב מתכנות היישומים ב-Java. העקרון העיקרי של בדיקת קיות המצוי בקוד הוא לוודא שהספרייה קיימת לפני שננסה לבצע כל פעולה נוספת על הספרייה. כמו כן, דבר חשוב לציין הוא שיש לוודא גם את ההרשאות המתאימות לכתיבה וקריאה לספרייה, על מנת למנוע שגיאות בזמן הריצה של האפליקציה.

## See Also

- [Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [How to Check if a Directory Exists in Java](https://www.baeldung.com/java-check-directory-exists)
- [Java Permissions and Runtime Exceptions](https://docs.oracle.com/javase/tutorial/essential/io/check.html)