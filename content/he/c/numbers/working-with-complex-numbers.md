---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:37.547316-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7 \u05DE\
  \u05DE\u05E9\u05D9 \u05D5\u05DE\u05D7\u05DC\u05E7 \u05D3\u05DE\u05D9\u05D5\u05E0\
  \u05D9, \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD \u05DE\u05D9\u05D5\u05E6\u05D2\u05D9\
  \u05DD \u05DB-`a + bi` \u05DB\u05D0\u05E9\u05E8 `i` \u05D4\u05D5\u05D0 \u05D4\u05E9\
  \u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\u05D5\u05E2\u05D9 \u05E9\u05DC `-1`.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD\
  \ \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\
  \u05D9\u05DD \u05D1\u05EA\u05D7\u05D5\u05DE\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:13.619188-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7 \u05DE\
  \u05DE\u05E9\u05D9 \u05D5\u05DE\u05D7\u05DC\u05E7 \u05D3\u05DE\u05D9\u05D5\u05E0\
  \u05D9, \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD \u05DE\u05D9\u05D5\u05E6\u05D2\u05D9\
  \u05DD \u05DB-`a + bi` \u05DB\u05D0\u05E9\u05E8 `i` \u05D4\u05D5\u05D0 \u05D4\u05E9\
  \u05D5\u05E8\u05E9 \u05D4\u05E8\u05D9\u05D1\u05D5\u05E2\u05D9 \u05E9\u05DC `-1`.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD\
  \ \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\
  \u05D9\u05DD \u05D1\u05EA\u05D7\u05D5\u05DE\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מספרים מרוכבים מורכבים מחלק ממשי ומחלק דמיוני, כאשר הם מיוצגים כ-`a + bi` כאשר `i` הוא השורש הריבועי של `-1`. מתכנתים עובדים עם מספרים מרוכבים בתחומים שונים כגון הנדסת חשמל, חישוב קוונטי ודינמיקת זורמים, נצלים את תכונותיהם הייחודיות לצורכי סימולציות, עיבוד אותות ופתרון סוגים מסוימים של משוואות מתמטיות.

## איך לעשות:

ב-C, מספרים מרוכבים נתמכים על ידי ספריית התקן, ובפרט `<complex.h>`. כדי להשתמש בהם, יש להצהיר על משתנים עם הסוג `double complex` (או `float complex` לדיוק בודד). הנה כיצד לבצע פעולות בסיסיות:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // הצהרה על מספר מרוכב 1+2i
    double complex z2 = 1.0 - 2.0*I; // הצהרה על מספר מרוכב נוסף 1-2i
    
    // חיבור
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // פלט: Sum: 2.00 + 0.00i

    // כפל
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // פלט: Product: 5.00 + 0.00i

    // מצורע מרוכב
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // פלט: Conjugate of z1: 1.00 - 2.00i
    
    // גודל
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // פלט: Magnitude of z1: 2.24

    // פאזה
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // פלט ברדיאנים
    
    return 0;
}
```
שימו לב ש-`I` הוא קבוע המייצג את היחידה הדמיונית ב-`<complex.h>`. פונקציות כמו `creal()` ו-`cimag()` מחלצות את החלקים הממשי והדמיוני, בהתאמה, בעוד ש-`conj()` מחשב את המצורע המרוכב. לצורך חישוב הגודל והפאזה (הארגומנט) של מספרים מרוכבים, משתמשים ב-`cabs()` וב-`carg()`.

## צלילה עמוקה

התמיכה במספרים מרוכבים ב-C היא יחסית חדשה, והיא נכללה בתקן C99. לפני כן, חישובים עם מספרים מרוכבים ב-C היו מורכבים ולעיתים דרשו מבני נתונים ופונקציות מותאמות אישית. הכללתו של `<complex.h>` וסוגי הנתונים המרוכבים תרמו תרומה ניכרת ליכולות השפה ליישומים מדעיים והנדסיים. עם זאת, כדאי לציין שיש שפות, כמו Python, שמציעות תמיכה אינטואיטיבית יותר במספרים מרוכבים באמצעות סוגי נתונים מובנים וספריית פונקציות עשירה יותר. למרות זאת, הביצועים והשליטה המוצעים על ידי C הופכים אותה לבחירה מועדפת למשימות חישוב גבוהות ביצועים, גם אם זה אומר להתמודד עם תחביר מעט יותר מסורבל לחישובים מרוכבים.
