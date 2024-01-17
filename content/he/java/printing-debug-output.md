---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Java: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# מה ולמה?
הדפסת פלט דיבאג היא פעולה שבה מתכנתים מדפיסים מידע נוסף בעת הרצת הקוד שלהם. הדבר הזה עוזר להבין כיצד הקוד פועל ותורם לזיהוי ותיקון באגים.

# כיצד לעשות זאת:
כדי להדפיס פלט דיבאג בקוד JavaScript, יש להשתמש בפעולת console.log(). למשל:

```java
int num1 = 5;
int num2 = 10;
System.out.println("The sum of " + num1 + " and " + num2 + " is: " + (num1 + num2));
```

פלט:

The sum of 5 and 10 is: 15

# כיום ישנם גם אפשרויות נוספות להדפסת פלט דיבאג, כגון השתמשות במנוע חיפוש אינטרנט כדי למצוא באגים בקוד או בשימוש בתוכנות ספציפיות לכיוון זה. אך למרבה המזל, השיטה הקלאסית של הדפסת פלט דיבאג עדיין הולכת וטובה ומספקת למתכנתים מידע חשוב על הקוד שלהם.

# העמקה:
עד לשנות ה-60 של המאה הקודמת, המתכנתים היו משתמשים בטכניקות מסורתיות יותר כדי למצוא ולתקן את באגיהם, כגון לתייג קטעי קוד עם תגיות או להדפיס מידע נוסף באמצעות מכתבים מיוחדים. אך עם התפתחות הטכנולוגיות, הדפסת פלט דיבאג צפתה בשיפורים והפכה למאפשרת למתכנתים לתקן באגים ולתחזק את הקוד שלהם בצורה יעילה יותר.

# ראה גם:
- https://www.geeksforgeeks.org/debugging-in-java/
- https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javac.html