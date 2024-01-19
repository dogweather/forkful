---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
המרת מחרוזת לאותיות קטנות מתארת את פעולת שינוי כל אותיות הגדולות במחרוזת לאותיות קטנות. תכנתים מבצעים זאת כדי להפוך את השוואת המחרוזות לאינסוניטיבית.

## איך לעשות:
נשתמש בפקודה tolower מהספרייה ctype.h. הנה דוגמה:
```C
#include <stdio.h>
#include <ctype.h>

void stringToLower(char str[]) {
    for(int i = 0; str[i]; i++){
        str[i] = tolower(str[i]);
    }
}

int main() {
    char myString[] = "Hello, World!";
    stringToLower(myString);
    printf("%s", myString);
    return 0;
}
```
הפלט של קטע הקוד הזה יהיה:
```C
hello, world!
```
זהו.

## עומק 
הפקודה tolower של הספרייה ctype.h שייכת ל שפת תכנות C המוקדמת ביותר. ישנן שפות תכנות אחרות שמציעות בנאים מורכבים יותר להמרת מחרוזות לאותיות קטנות, אך הפונקציה tolower מהספרייה ctype.h עדיין האופציה הפשטנית ביותר לשפת C.

לגבי פרטי ביצוע למונחים מתקדמים יותר, tolower מבזבז O(n) זמן ביצוע (n מייצג את אורך המחרוזת), אך זה לא הבעיה ברוב המקרים.

## ראה גם:
[תיעוד הספרייה ctype.h](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
[מדריך שפת C](https://www.learn-c.org/)
[המרת מחרוזת לאותיות קטנות בשפות תכנות אחרות](https://stackoverflow.com/questions/2667838/how-do-i-lowercase-a-string-in-c)