---
title:                "שימוש ראשי במחרוזת"
html_title:           "Java: שימוש ראשי במחרוזת"
simple_title:         "שימוש ראשי במחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

מה ולמה?
כאשר מתבצע אכנסה של טקסט למערכת, קפיטלייזציה היא תהליך של המרת האותיות הקטנות של המילים לאותיות גדולות בתחילת כל מילה. תהליך זה משמש לאיזון ודפוס מילים בתוך הטקסט ומקל על קריאתו. תיכנותנים מבצעים קפיטלייזציה כדי לשפר את קריאות הקוד ולהרוויח עדיפות בביצועים.

איך לבצע
לביצוע קפיטלייזציה ב-Java ישנם מספר דרכים. אחת הדרכים הנפוצות לבצע קפיטלייזציה היא להשתמש בפונקציה toUpperCase עם המחרוזת הרצויה כפרמטר. לדוגמה:
```Java
String text = "hello world";
String capitalized = text.toUpperCase(); // עכשיו נוכל להשתמש במשתנה capitalized עם המחרוזת שהתווספה
System.out.println(capitalized);
// פלט: HELLO WORLD
```

עוד דרך לבצע קפיטלייזציה היא להשתמש בפונקציה toCharArray כדי לגשת אל מערך התווים של המחרוזת ולשנות את האותיות הרלוונטיות לגדולות. כדי להחזיר מחרוזת, ניתן להשתמש במתודת valueOf. לדוגמה:
```Java
String text = "hello world";
char[] charArray = text.toCharArray(); // מוצאים את המערך של התווים של המחרוזת
charArray[0] = Character.toUpperCase(charArray[0]); // הפעלת פונקציה על התו הראשון במערך
String capitalized = String.valueOf(charArray); // המרת המערך למחרוזת והצגתה במשתנה חדש
System.out.println(capitalized);
// פלט: Hello world
```

עומק
בעבר, היו תכנותנים שמשתמשים בטקסט מקושר כדי לבצע קפיטלייזציה, אבל כיום מומלץ להשתמש בפונקציות המובנות בשפות תכנות כמו Java. כמו כן, ישנן גם פתרונות אחרים כמו שימוש בספריות חיצוניות או כלי בעזרת הקפיטלייזציה.

ראו גם
למידע נוסף על קפיטלייזציה ב-Java, ניתן לעיין במסמכי המחשבה המפורטים באתר הרשמי של Java.