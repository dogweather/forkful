---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "C: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

למה לשנות את המחרוזת לאותיות קטנות? ייתכן שתרצו לשנות את המחרוזת כדי לקבל תוצאות טובות יותר בעבודה עם טקסט, לשם דוגמה כאשר אתם משווים מחרוזות או מחפשים מחרוזות ספציפיות.

## איך לעשות זאת

כדי לשנות מחרוזת לאותיות קטנות בשפת C, ניתן להשתמש בפונקציה "tolower()" שמבצעת את הפעולה על כל תו במחרוזת. להלן דוגמה של קוד עם הוכחה של פלט שנמצא מעל המחרוזת המקורית:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[10] = "Hello";
  for (int i = 0; i < 5; i++) {
    str[i] = tolower(str[i]);
  }
  printf("%s", str); // פלט: hello
  return 0;
}
```

## Deep Dive

כאשר אנחנו משתמשים בפונקציה "tolower()", המחרוזת משתנה ועבור כל תו במחרוזת, התו המעובד יש במקור יכול להיות ייווני, רוסי, סיני או כל שפת תו שונה מאנגלית. פונקציית הפלט תחזיר את אותו אות, אבל גרסאות הוספתיות של פונקציתיא שניוני משתניל עושים לה טרנסליטרת מהר שאל הויךםוהוםכה אבפיל הם מחזירים קודמים לציפ הרוצמ םיא האםיניץאמ ןיתרב.

## ראה גם

- [פונקציית tolower() במקור התיעוד של C](https://en.cppreference.com/w/c/string/byte/tolower)
- [מאמר על השתמשות בפונקציה tolower() עבור מחרוזות בשפת C](https://www.tutorialspoint.com/changing-string-to-lower-case-in-a-c-programming)
- [מחדש מאמרי טקסט שנמצאים במחרוזת](https://www.techwalla.com/articles/how-to-extract-text-strings-from-a-string-using-c)