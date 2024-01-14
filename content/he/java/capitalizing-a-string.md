---
title:                "Java: שינוי רישיות במחרוזת"
simple_title:         "שינוי רישיות במחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה

ניתן להשתמש בפונקציה להפיכת אותיות במילה בצורה מקומית בשפת ג'אווה על מנת ליצור טקסט בכיתה גדולה וחזקה יותר. זה יכול להיות שימושי כאשר אנו רוצים להדגיש מספר כתבי דפוס או מילים מסוימות בתוך טקסט, או כאשר אנו רוצים לסמן כותרות וכותרות כבוד לצורך מיון המידע.

# איך לעשות זאת

כדי להפעיל את הפונקציה להפיכת אותיות על מתחלת המילה, ניתן להשתמש בפעולה הפשוטה הבאה:

```java
String capitalize(String text) {
    return text.substring(0, 1).toUpperCase() + text.substring(1);
}
```

בדוגמה זו, אנו משתמשים בפעולת `substring()` כדי לקחת את האות הראשונה במילה ולהפוך אותה לאות גדולה באמצעות `toUpperCase()`. לאחר מכן, אנו משתמשים שוב ב־`substring()` כדי להשאיר את יתר המילה כפי שהיא.

כדי להפעיל את הפונקציה על מבחר מילים תוך השמירה על כמויות האותיות המקוריות, ניתן להשתמש בפעולה הבאה:

```java
String capitalizeAll(String text) {
    String[] words = text.split(" ");
    StringBuilder result = new StringBuilder();
    for (String word : words) {
        result.append(capitalize(word)).append(" ");
    }
    return result.toString().trim();
}
```

בדוגמה זו, אנו משתמשים בפעולת `split()` כדי להפריד את המילים לפי רווחים ולהפעיל את הפונקציה `capitalize()` על כל אחת מהן. לאחר מכן, אנו משתמשים במחלקת `StringBuilder` כדי ליצור מחרוזת חדשה ולהוסיף לה את המילים המפונקתות ביחד עם רווחים ביניהן. לבסוף, אנו משתמשים בפעולת `trim()` כדי להסיר רווח סופי שלא נדרש.

# חקירה מעמיקה

הפונקציה להפיכת אותיות נחשבת ליעיל