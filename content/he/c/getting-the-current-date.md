---
title:                "קבלת התאריך הנוכחי"
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

יתרון עיקרי לקבלת תאריך נוכחי בתכנות בשפת C הוא לצורך מניפולציות על תאריכים, כגון חישוב עיקרי החודש או ייצוג זמן נוח לקריאה למשתמש.

## איך לעשות

החלק הזה יעסוק בקוד דוגמה לקבלת התאריך הנוכחי בשפת C ואת הפלט המצורף אליו.

```c
#include <stdio.h>
#include <time.h>

int main(void)
{
    // משתנה k המחזיק את התאריך הנוכחי בפורמט תוך כדי ביטוי בעזרת מניפולטור הזמן (time.h)
    time_t k = time(NULL);

    // יצירת משתנה בעל מצביע לתוצאת הוראת היצירה של התאריך נוכחי
    struct tm* result = localtime(&k);

    // הדפסת התאריך הנוכחי בתבנית פשוטה לקריאה
    printf("%.2d/%.2d/%.2d\n", result->tm_mday, result->tm_mon + 1, result->tm_year + 1900);

    return 0;
}

// פלט: 08/09/2021
```

## מעמקים

במקרים רבים, כאשר זמן כלשהו נדרש לתכנית, חשוב לגלות תאריך נוכחי בכדי לפעול עליו או להדגיש עליו. בכדי לקבל תאריך נוכחי בשפת C, ניתן להשתמש במניפולטור הזמן המצוי בספריית הסטנדרטית `time.h`. הוא מכיל פונקציות המאפשרות לקבלת והתאמת זמנים, תאריכים ומבני תאריכים שונים.

## ראה גם

- [מדריך לספריית `time.h` בשפת C](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)
- [מדריך למניפולטור הזמן בשפת C](https://www.cprogramming.com/tutorial/time.html)