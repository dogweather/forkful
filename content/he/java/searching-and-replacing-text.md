---
title:                "חיפוש והחלפת טקסט"
html_title:           "Java: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הוא תהליך המאפשר למצוא ולהחליף מחרוזות טקסט בקוד המתוכנת. פעולה זו מאפשרת למפתחים לשנות את המחרוזת המקורית ולהתאים אותה לצורכי התוכנית בקלות ומהירות.

## כיצד לעשות?
```Java
// פתיחת קובץ טקסט קיים לקריאה והכנסתו למשתנה טקסט
File file = new File("myfile.txt");
Scanner scanner = new Scanner(file);

// עבור כל שורה בקובץ, קריאה והכנסתה למשתנה שורה
while (scanner.hasNextLine()) {
    String line = scanner.nextLine();

    // חיפוש והחלפת המחרוזת המקורית לפי הכללים שנקבעו
    String newLine = line.replaceAll("oldString", "newString");
    
    // הדפסת השורה המעודכנת
    System.out.println(newLine);
}

scanner.close(); // סגירת הקובץ
```

פלט:
```
This is a sample text.
```

## חפירה עמוקה
חיפוש והחלפת טקסט הינם תוספת חשובה לתוכנית כתיבת הקוד. פעולה זו פותרת בעיות בקוד המתוכנת ומאפשרת לעשות שינויים בצורה נוחה ומהירה. כמו כן, אלגוריתם זה נחשב לאחד הרכיבים החשובים ביותר בלימוד תכנות.

אפשרויות אלטרנטיביות כאשר מדובר בחיפוש והחלפה של טקסט הן חיפוש ידני או שימוש בתוכנת עיבוד טקסט. חיפוש והחלפת טקסט בקוד יכול לאפשר היקף גדול יותר של שינויים ולהפוך את התהליך לנוח ויעיל.

## ראה גם
למידע נוסף על חיפוש והחלפת טקסט ב-Java, ניתן לבדוק את המקורות הבאים:

- [תיעוד רשמי של חיפוש והחלפת טקסט ב-Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll%28java.lang.String,%20java.lang.String%29)
- [פוסט בבלוג על חיפוש והחלפת טקסט ב-Java](https://www.baeldung.com/string-replaceall-java)