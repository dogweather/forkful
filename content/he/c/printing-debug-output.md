---
title:                "הדפסת פלט איתור תקלות"
html_title:           "C: הדפסת פלט איתור תקלות"
simple_title:         "הדפסת פלט איתור תקלות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

הדפסת פלט הודעות שגיאה (debug output) היא כלי חשוב לתיקון תקלות בקוד. היא מאפשרת למתכנת לראות את מהלך הקוד ואת הערכים של המשתנים בזמן הריצה, מה שעוזר למצוא את הבעיות ולתקן אותן באופן מהיר ויעיל.

## איך להשתמש בדפסת פלט הודעות שגיאה

כאשר משתמשים בשפת תכנות C, ניתן להשתמש בפונקציית `printf()` כדי להדפיס פלט לקונסול. לפני השתוללות (debugging), כדאי להוסיף פקודת התניה (conditional statement) כדי לסנן רק את הפלט הרלוונטי לזמן התקלה.

```C
if (DEBUG) {
  printf("Value of x is %d\n", x);
  printf("Value of y is %d\n", y);
}
```

כאשר מריצים את הקוד, הפלט יודפס רק כאשר התנאי `DEBUG` מתקיים. זה מאפשר להפעיל ולבטל את הדפסת הפלט בקלות.

## הנחה עמוקה

אחת השיטות הטובות ביותר לייצר פלט דיבוג הוא לשים לב לסדר של הפלט המודפס. לדוגמה, ניתן להתחיל בהדפסת שורת פרידה (`separator line`) עם כותרת שנמצאת למטה. ניתן גם להוסיף מידע נוסף כמו שם הפונקציה שמדפיסה את הפלט והזמן שהתרחשה התקלה.

```C
printf("/=============== DEBUG OUTPUT ===============/\n");
printf("Function: calculate_sum()\n");
printf("Timestamp: %ld\n", time(NULL));
printf("Value of x is %d\n", x);
printf("Value of y is %d\n", y);
```

כמו כן, יש לזכור להסיר את ההדפסות לפני גרסת הפוקציה הסופית. חשוב לזכור שהדפסות יכולות להוריד את ביצועי הקוד ולהאט את התכנית, לכן חשוב להשתמש בהן רק בזמן מתקנת התקלה.

## ראה גם

- [C Debugging Techniques](https://