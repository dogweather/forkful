---
title:                "Java: קריאת ארגומנטים משורת הפקודה"
simple_title:         "קריאת ארגומנטים משורת הפקודה"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##מדוע
אדם יבחר לקרוא ארגומנטים של שורת פקודה רק במקרים מסוימים בסיסיים?

##איך לעשות זאת
 ` ``Java
public static void main(String[] args) {
  // Declare main method
  for(int i = 0; i < args.length; i++) {
    // Use a for loop to iterate through the arguments
    System.out.println("Argument " + i + ": " + args[i]);
    // Print out each argument along with its index
  }
}
```

הפלט המשוערך יחזיר:

`Argument 0: argument1`<br/>
`Argument 1: argument2`<br/>
`...`

##טיול עמוק
קריאת ארגומנטים של שורת פקודה היא כלי מאוד נוח כאשר אנו רוצים לתקשר ישירות עם המשתמש ולהעביר לו פרמטרים לתכנית שלנו. בדרך כלל, כאשר אנו בונים אפליקציה גרפית איתנה ממשק משתמש, אנו משתמשים בקריאת פרמטרים של שורת פקודה כדי להפעיל פעולות ולשנות תכנית באופן דינאמי.

##ראו גם
- [Java שמות של בינתחומיים עבור פקודת מערכת בחלונות](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [השרות העליון לכתיבת אפליקציות טרמינלית גרפיות בשפת Java](https://eclipse.org/efxclipse/index.html)
- [מדריך לעבודה עם פקודות מערכת עם Java](https://www.tutorialspoint.com/java/util/java_util_scanner.htm)