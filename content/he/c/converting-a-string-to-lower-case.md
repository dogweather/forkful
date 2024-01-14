---
title:                "C: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

המרת מחרוזת לאותיות קטנות היא פעולה נפוצה בתכנות בשפת C. מספר סיבות לכך כוללות קלות בקריאה והשוואה של מחרוזות, ואפשרות לטפל בטעויות של משתמשים בחיפושים ופילטרים. בכתב הזה, אני אראה לכם איך להמיר מחרוזת לאותיות קטנות בשפת C ומה האפשרויות שלנו.

## איך לעשות זאת

כדי להמיר מחרוזת לאותיות קטנות בשפת C, ישנן שתי אפשרויות עיקריות - פילוח המחרוזת לאותיות בינדיקס, או שימוש בפונקציית `tolower()`. בהמשך תמצאו קוד המדגים את שתי האפשרויות.

### פילוח לאותיות בלולאה

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "HELLO WORLD";
    int i;

    for (i = 0; str[i] != '\0'; i++) {
        if (isupper(str[i])) {
            str[i] = tolower(str[i]);
        }
    }

    printf("%s\n", str);

    return 0;
}

// Output:
// hello world
```

כאן אנו בודקים אות אות אם היא אופקרית בעזרת הפונקציה `isupper()` ואם כן, משנים אותה לאות קטנה בעזרת `tolower()`.

### שימוש בפונקציית tolower()

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char str[] = "HELLO WORLD";
    int i;

    for (i = 0; str[i] != '\0'; i++) {
        str[i] = tolower(str[i]);
    }

    printf("%s\n", str);

    return 0;
}

// Output:
// hello world

```

בכאן נעבור בלולאה על כל אותיות המחרוזת ונשתמש ישירות בפונקציית `tolower()` ללא צורך בבדיקה נוספת.

## חקירה עמוקה

כאשר מעבירים מחרוזת לאותיות קטנות, צריך לקחת בחשבון שזו פעולה לא משנה. כלומר, אם כן יתכן שהמחרוזת כבר נמצאת באותיות קטנות, אז לא יהיו שינויים בפלט.

בנוסף, כאשר משתמשים ב