---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:40:20.267388-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

הסרת מרכאות ממחרוזת אומרת להוציא את כל סימני הציטוט—בין אם מדובר בציטוט בודד (') או כפול (")—שהם חלק מתוכן המחרוזת. מתכנתים עושים זאת כדי לנקות קלט, להכין נתונים לעיבוד נוסף, או למנוע שגיאות תחביר כאשר מתמודדים עם נתיבי קבצים ופקודות בשפות שמשתמשות במרכאות לציון מחרוזות.

## איך לעשות:

הנה פונקציה בשפת C שתנקה את אותם מרכאות מעצבנים מהמחרוזות שלך:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitized: %s\n", str);
    return 0;
}
```

דוגמה לפלט:

```
Original: He said, "Hello, 'world'!"
Sanitized: He said, Hello, world!
```

## ניתוח מעמיק

הסרת מרכאות ממחרוזת היא משימה שקיימת מראשית התכנות, שם היגיינת הנתונים הייתה ועדיין מפתח להימנעות משגיאות (כמו פרצות SQL Injection) או להבטיח שמחרוזת יכולה בבטחה להעבר למערכות שעשויות לבלבל בין ציטוט לתו בקרה.

בהיסטוריה, שפות שונות טיפלו במשימה זו באופן שונה—חלק מהן כוללות פונקציות מובנות (כמו `strip` ב-Python), בעוד שאחרות, כמו C, דורשות יישום ידני בשל התמקדותן בנתינת שליטה ברמה נמוכה יותר למפתחים.

חלופות כוללות שימוש בפונקציות ספרייה כמו `strpbrk` למציאת מרכאות או שימוש בביטויים רגולריים (עם ספריות כמו PCRE) לדפוסים מורכבים יותר, אף על פי שזה עשוי להיות יתר על המידה עבור הוצאת מרכאות בלבד.

היישום שלמעלה פשוט סורק כל תו במחרוזת, מעתיק רק תווים שאינם מרכאות למיקום המצביע הכותב. זה יעיל מאחר שהתהליך נעשה במקום ללא צורך בזיכרון נוסף עבור המחרוזת התוצאה.

## ראה גם

- [פונקציות ספרייה סטנדרטיות של C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [הבנת מצביעים ב-C](https://www.learn-c.org/en/Pointers)
- [כתיבת קוד בטוח ב-C](https://owasp.org/www-project-secure-coding-in-c)