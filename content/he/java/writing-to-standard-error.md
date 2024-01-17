---
title:                "כתיבה לתקליט התקשרות תקניים"
html_title:           "Java: כתיבה לתקליט התקשרות תקניים"
simple_title:         "כתיבה לתקליט התקשרות תקניים"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# מה ולמה?

כתיבה לפלט שגיאה נורמלי היא כתיבה של מידע מהקוד לצד שגיאה כאשר התוכנית מריצה בסביבת Java. כתיבה לפלט שגיאה מאפשרת למתכנת לזהות את המקומות בקוד שעלולים לגרום לשגיאות ולתקן אותם באופן מהיר ויעיל.

# איך לבצע:

כתיבה לפלט שגיאה ב-Java נעשית על ידי השתמשות בפקודת System.err.println() והעברת הנתונים הרצויים לפרמטר כפלט. הנה דוגמא פשוטה:

```Java
double x = 10.5;
System.err.println("An error has occurred! The value of x is: " + x);
```

פלט:

```
An error has occurred! The value of x is: 10.5
```

ניתן גם להשתמש בפונקציות מתקדמות יותר כגון System.err.print() שמאפשרת להדפיס כל סוג של נתון כפלט.

# מעמקים נמוכים:

תהליך זה של כתיבה לפלט שגיאה נוצר בשנות ה-70 כאשר הותקנו מכשירי ניתוח תקינה שיעזרו למתכנתים לזהות ולתקן שגיאות בשורה אחת. בימים אלה, ישנם כלים נוספים שעוזרים למתכנתים ללכת מעבר לפשטות של כתיבה לפלט שגיאה, כגון מעקב כימותרי שמסייע באיתור שגיאות בקוד מורכב.

# ראה גם:

מאמר זה מסכם את השימוש של כתיבה לפלט שגיאה בתוכניות בשפת Java. לקריאה נוספת על הנושא, ניתן להתחיל בלינקים הבאים:

- כתיבה לפלט שגיאה ב-Java: https://www.javatpoint.com/how-to-print-message-to-error-stream-in-java
- כתיבת פלט שגיאה לסטרים נפרד: https://www.codingame.com/blog/java-stream-to-stderr
- הבדלים בין כתיבה לפלט שגיאה לפלט רגיל: https://stackoverflow.com/questions/4192173/what-is-the-difference-between-system-out-println-and-system-err-println-in-jav