---
title:                "בדיקת קיום תיקייה במחשב"
html_title:           "Java: בדיקת קיום תיקייה במחשב"
simple_title:         "בדיקת קיום תיקייה במחשב"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

קוד שיעורים - בדיקת קיום תיקייה ב-Java

## מה ולמה?
בדיקת קיום תיקייה ב-Java היא פעולה שמאפשרת למתכנתים לבדוק האם תיקייה קיימת במערכת הקבצים או לא. הפעולה הזאת חשובה מאוד בעבודת התכנות, מאחר והיא מאפשרת לנו להתאים את התכנית שלנו לתנאי מסוימים.

## איך לבדוק קיום תיקייה?
כדי לבדוק האם תיקייה קיימת ב-Java יש להשתמש בפקודת `exists()` על אובייקט מסוג `File`. הנה דוגמה פשוטה של קוד המדגים את השימוש בפקודה זו:

```Java
import java.io.File;

public class Main {
  public static void main(String[] args) {
    File myDir = new File("path/to/directory");
    if (myDir.exists()) {
      System.out.println("התיקייה קיימת במערכת הקבצים!");
    } else {
      System.out.println("התיקייה לא קיימת במערכת הקבצים.");
    }
  }
}
```

תוצאת הקוד הזה יתקבל כך:
```
התיקייה קיימת במערכת הקבצים!
```

## מה אנחנו יודעים על בדיקת קיום תיקייה?
בדיקת קיום תיקייה היא פעולה יסודית בתכנות ומשמשת בכלל המערכות המבוססות על קבצים. הפעולה הזאת נאמנה ויעילה ביותר, והיא תמיד תציג את התוצאה הנכונה. ישנן גם פקודות נוספות ב-Java שמאפשרות לנו לבצע בדיקת קיום תיקייה, כגון `isDirectory()` ו-`canRead()`, אך `exists()` היא הפקודה הבסיסית והנפוצה ביותר.

## רוצים לדעת עוד?
אם ברצונכם לקרוא יותר על הפקודה `exists()` ב-Java, כדאי לבקר במקורות המוצעים מטה:
- [המדריך המפורט של Oracle על הפקודה `exists()`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--) - מכיל מידע טכני על הפקודה זו ודוגמאות לשימוש בה.
- [פוסט באתר GeeksforGeeks על ניהול קבצים ב-Java](https://www.geeksforgeeks.org/file-exists-method-in-java-with-examples/) - מסביר על הפקודה `exists()` ומכיל דוגמאות לשימוש בה.
- [פסקה בויקיפדיה על היסטוריה של הפקודה `exists()`](https://en.wikipedia.org/wiki/Exists_(file_system)) - מכיל מידע על התולדות של הפקודה ואיך היא נועדה לפעול במקור.