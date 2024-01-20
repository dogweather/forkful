---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה & למה?
הפקת מספרים אקראיים בתכנות היא דרך ליצור מספרים אשר לא ניתן לחזות את הערך שלהם מראש. מפתחים משתמשים בה במגוון מקרים, כמו משחקים, בדיקות, אבטחה ותצוגת מידע אקראי.

## איך לכתוב:
הנה דוגמה של קוד בשפת C שמייצר מספר אקראי:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
    srand(time(0)); // Initialization
    int randNum = rand(); // Generates a random number

    printf("%d\n", randNum); // Prints the random number

    return 0;
}
```
בעת ריצה, הקוד ידפיס מספר אקראי שונה בכל פעם.

## Deep Dive
הפקת מספרים אקראיים הייתה חשובה מאז הימים הראשונים של התכנות. למדנו שאין משהו כזה "אקראיות אמת" במחשב, אך ניתן להשיג "אקראיות תוך כדי" באמצעות שימוש בפונקציות מתאימות כמו `rand()`. ישנם חלופות להפקת מספרים אקראיים, כמו `random()` או `drand48()`, אך הבחירה נותרת למפתח. 

## See Also
לקבלת מידע נוסף והגדרות מורחבות, הקישורים הבאים יכולים להיות שימושיים:
- [דוקומנטציה של הפונקציה rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [רעיון מאחורי אקראיות במחשב](https://medium.com/dev-genius/there-is-no-such-thing-as-a-random-number-d284759f4dae)