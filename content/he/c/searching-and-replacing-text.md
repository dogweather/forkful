---
title:                "C: חיפוש והחלפת טקסט"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה
מבוא
חיפוש והחלפה של טקסט הוא כלי חשוב בתכנות, שימוש בו עשוי לקצר את תהליך העדכון ושדרוג של קוד ולשפר את הקריאות והנוחות של קוד. בפוסט הזה אנחנו נראה איך להשתמש בפיתרון חיפוש והחלפת טקסט כדי לכתוב קוד בצורה יעילה יותר ולהפעיל את הפתרון המתאים ביותר לצרכים שלנו.

## כיצד לעשות זאת
לכנות מחרוזות
>We will use the `strncpy()` function, which copies the characters of one string into another, up to a given limit.

כדי להשתמש בפונקציה, נצטרך לבצע שימוש בהתקנת ה `` `string.h` ``, בעזרת `` `#include to``', "בכדי לכלול את הפונקציה =להשתמש נשתמש ++

 נתחיל עם המקרה הפשוט, נרצה להחליף כל מופע של המחרוזת `"hello"` עם המחרוזת `"hi"`. נתחיל עם משתני `from_str` ו-`to_str` שאליהם נשמר את המחרוזת `hello` ו-`hi` בהתאמה.
```
#include <string.h>

int main() {
   char from_str[] = "hello";
   char to_str[] = "hi";
}
```
השתמשו ב-`strchr()` function כדי לקבוע את מיקום המחרוזת בתוך המשתנה, ואז החליפו אותו באמצעות `strncpy()` function והרחבה `while` loop עם מתווה `strlen()` כדי להחליף את כל פעם שרקשים של המחרוזת הראשונה:

```
#include <stdio.h>
#include <string.h>


int main() {
   char sample_str[] = "This is a sample string";
   char from_str[] = "string";
   char to_str[] = "word";

   char *from_ptr = from_str;
   char *to_ptr = to_str;

   while((to_ptr=strchr(sample_str, *from_ptr))) {
      strncpy(to_ptr, to_str, strlen(from_str));
      to_ptr += strlen(from_str);
      ++from_ptr;
   }

   printf("%s\n", sample_str);
   return(0);
}
```

רצוי גם לבדוק את קונפיגורציות הפרמטר כמו גידול התו "המחרוזת שאנחנו רוצים להחליף אותה ואז נתאים אותה פונדריתה".

## לנסות
החלפת כל תו קיום
מתכונ