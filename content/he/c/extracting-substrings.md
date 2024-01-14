---
title:    "C: יציאת תת מחרוזות"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

מחרוזות הן חלק חשוב בתכנות בשפת C. לעיתים קרובות נתקלים במצבים בהן נצטרך להפעיל פעולות על אחת או יותר מתתי-מחרוזות של מחרוזת גדולה יותר. לפיכך, השימוש בפעולות של החיתוך של מחרוזות נחשב למנגנון חשוב לתכנות ב-C.

## איך לעשות זאת

ישנן מספר דרכים לחתוך מחרוזות בשפת C. הפעולה הכי פשוטה היא להשתמש בפונקציה "strncpy", שמקבלת שלושה פרמטרים: המחרוזת המקורית, מחרוזת יעד, ומספר התווים המקסימלי להעתקה. לדוגמה:

```C
char source[] = "שלום עולם";
char destination[6];

strncpy(destination, source, 5);
printf("%s", destination);
```

תוצאה: "שלום"

כמו כן, ניתן להשתמש בפונקציות נוספות כמו "strtok" ו-"strchr" לחיתוך מחרוזות לפי תווים מסוימים או לפי תתי-מחרוזות. כדאי לקרוא את התיעוד המקורי של C כדי להבין את הפונקציות הללו בצורה מלאה יותר.

## חקירה עמוקה

כעת שיזכרו את הדרכים השונות לחיתוך מחרוזות בשפת C, ניתן ללמוד כיצד הפונקציות האלה מובנות וממומשות בתוך השפה. כמו כן, ניתן לחקור מקרים מיוחדים כמו מחרוזות בפורמטים מיוחדים ואיך לחתוך אותן במקרה כזה.

למידע נוסף על מנגנוני חיתוך מחרוזות בשפת C, ניתן לעיין במאמרים המצורפים:

- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Learn C String Manipulation with Examples](https://www.educba.com/c-string-manipulation/)
- [Manipulating Strings in C](https://www.geeksforgeeks.org/strings-in-c-2/)

## ראה גם

- [תיעוד מלא על פונקציות מחר