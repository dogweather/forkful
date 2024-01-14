---
title:    "C: כתיבה לתקליט התקן"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבה לפלט שגיאה (standard error) הוא חלק חשוב מתהליך התכנות בשפת C. יכול להיות שזו הדרך היחידה להתריע על שגיאות בזמן ריצה של התוכנית, ולכן כדאי לדעת כיצד לעשות זאת.

## איך לעשות זאת

כדי לכתוב לפלט שגיאה, ניתן להשתמש בפונקציית `fprintf()` ולהגדיר את ה `stderr` כפרמטר. לדוגמה:

```C
fprintf(stderr, "הודעת שגיאה");
```

הפלט יופיע כאשר התוכנית תירתע עקב שגיאה.

## חקירה מעמיקה

כדי להבין טוב יותר את כתיבה לפלט שגיאה, כדאי להתעמק במנגנונים של `stderr`. ה `stderr` מייצג את הפלט של השגיאות, בניגוד ל `stdout` שמייצג את הפלט הרגיל של התוכנית. כשתוכנית מטפלת בשגיאות, היא יכולה להשתמש ב `stderr` להדפסת הודעות עליהן.

ה `stderr` גם מאפשר לנו להפנות את הפלט לקובץ אחר, במקרה שאנחנו רוצים לשמור את השגיאות לקובץ מיוחד. על מנת לעשות זאת, ניתן לשנות את הפרמטר השני של ה- `fprintf()` לשם הקובץ שנרצה.

## ראו גם

- https://www.programiz.com/c-programming/c-file-input-output
- https://www.tutorialspoint.com/cprogramming/c_error_handling.htm