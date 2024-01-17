---
title:                "כתיבת בדיקות"
html_title:           "C: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
הכתיבת בדיקות היא כתיבת קוד המבדיל בין קוד נכון לקוד שגוי. הסיבה שתכנתנים עושים זאת הינה כי זה מאפשר להם לזהות ולתקן שגיאות מוקדם בעתיד ולהבטיח תמיכה רחבה יותר בתוכניות שלהם.

## איך לעשותזה
הנה דוגמה של כיצד לכתוב בדיקות בשפת C:

```C
#include <stdio.h>

int main() {
    // כתיבת תוכנית המדפיסה את המספר שלאחר מכן תכפיל אותו בעצמו
    int num = 5;
    int result = num * num;
    printf("התוצאה היא: %d", result);

    return 0;
}
```

הפלט המתואר הינו:
```
התוצאה היא: 25
```

## ירידה לעומק
תיאור המקור היסטורי של כתיבת בדיקות בשפת C נמצא בסיפור מעניין של הנדסאי החשמל ויליאם גילבר.
אפשרויות נוספות לכתיבת בדיקות כוללות ספריית האסמבלר Unity וספריית הבדיקות CppUnit.
מימוש הפונקציות הרלבנטיות לכתיבת בדיקות הינו נושא של בדיקה רחבה לתלמידי תכנות.

## ראה גם
- [מדריך לבדיקות קאפי](https://github.com/catchorg/Catch2)
- [דפדפן מתקדם בכתיבת בדיקות בשפת C](https://github.com/bendikro/plutoc)
- [ספריית GTest לכתיבת קוד בדיקות בשפת C++](https://github.com/google/googletest)