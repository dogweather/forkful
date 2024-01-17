---
title:                "מחברת מחרוזות"
html_title:           "C: מחברת מחרוזות"
simple_title:         "מחברת מחרוזות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# מה ולמה?
דיכונים של מחרוזות הם תהליך בו מחרוזות שונות משולבות יחד כדי ליצור מחרוזת אחת גדולה יותר. תהליך זה נמצא בשימוש רחב בתכנות, במיוחד כאשר נדרשת הדפסת מידע מרובה בפני המשתמש.

# איך לעשות?
ניתן ליצור דיכון בין מחרוזות בשפת C באמצעות השתמשות בפונקציית strcat. הנה מספר דוגמאות להרצה של הפונקציה:

```C
char hello[6] = "Hello";
char world[6] = "World";
strcat(hello, world);
printf("%s", hello);
```
פלט:
```
HelloWorld
```

ניתן גם להשתמש באופרטור השמה מפונקציית sprintf כדי ליצור דיכון חדש. לדוגמה:

```C
char welcome[50];
char language[10] = "Hebrew";
sprintf(welcome, "Welcome to %s!", language);
printf("%s", welcome);
```

פלט:
```
Welcome to Hebrew!
```

# מעמיקים
הדיכון של מחרוזות הוא תהליך שהתפתח בשנים האחרונות של המאה ה-19 עם התפתחות שפת התכנות C. כיום נוס התהליך בין שפות תכנות שונות וגם ישנן שיטות אחרות לסיבוב מחרוזות, כגון השמה ישירה ושימוש בפונקציות נוספות כגון strncat.

# ראו גם
למידע נוסף על הדיכון של מחרוזות בשפת C, ניתן לקרוא את טקסט המדריך המקוצר של Microsoft על strcat: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strcat-strcat-wcscat-mbscat-strlcat

למידע נוסף על שפת C בכלל, ניתן לקרוא את טקסט המדריך המקוצר של וויקיפדיה: https://he.wikipedia.org/wiki/C_%D7%AA%D7%9B%D7%A0%D7%95%D7%9F.