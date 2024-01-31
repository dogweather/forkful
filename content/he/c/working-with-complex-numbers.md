---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:38:13.356912-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מורכבים, שילוב של חלקים ממשיים ומדומים (כמו 3 + 4i), הם מפתח בחישובים מתקדמים, כמו עיבוד אותות או פתרון משוואות מסוימות. מתכנתים מתמודדים איתם ביישומים שמבוססים על מתמטיקה כבדה, שבהם המספרים המסורתיים לא מספיקים.

## איך לעשות:
C, מאז C99, כוללת סוג מורכב וספרייה ילידית. כך ניתן להשתמש בה:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // הצהרה על שני מספרים מורכבים
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // פעולות עם מספרים מורכבים
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // הדפסת התוצאות
    printf("Sum: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Product: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // ערך מוחלט & זווית הפאזה
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

דוגמא לפלט:
```
Sum: 3.0 + 1.0i
Product: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## טבילה עמוקה
מספרים מורכבים חוזרים אחורה מאות שנים, עם שורשים באלגברה של המאה ה-16. קדימה לעתיד, הם כעת חלק בלתי נפרד בשפות תכנות רבות, לא רק ב-C.

התקן C99 הציג את `<complex.h>`, כותרת המגדירה מקרואים, פונקציות, ואת סוג הנתונים `complex`. קיימות אלטרנטיבות - כמו ליצור מבנה משלך, אבל למה להמציא את הגלגל מחדש? ספריית הסטנדרט של C מותאמת ומוכנה לשימוש.

למרות הכוח שלו, התמיכה במספרים מורכבים ב-C אינה נטולת ביקורות. זה יכול להיות פחות אינטואיטיבי מאשר פיצ'רים דומים בשפות כמו Python, והתמודדות עם מקרי קצה יכולה להיות מסובכת. אבל לביצועים גולמיים, זה עדיין בחירה מוצקה.

## ראה גם
- תיעוד התקן C99 ל-`<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- תקן ה-IEEE לחישוב נקודה צפה (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- מדריך מקוון למתמטיקה של מספרים מורכבים ב-C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
