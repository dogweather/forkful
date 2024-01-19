---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לתבנית היא פעולה שבה מוחקים את כל התווים ממחרוזת שמתאימים לתבנית מסוימת. תכנתים עושים את זה כדי לנקות את המחרוזת מתוך שיקולים של ביצועים או כללים עסקיים.

## איך
נותח קוד להלן מצייג איך למחוק תווים שמתאימים לתבנית בשפת תכנות C באמצעות הפונקציה `strpbrk`. 

```C
#include<stdio.h>
#include<string.h>

int main() {
    char str[100] = "hello123";
    char *ptn = "1234567890";
    str[strcspn(str, ptn)] = '\0';
    printf("%s\n", str);
    return 0;
}
```
הפלט:
```
hello
```
אחרי הריצה של הקוד, התווים "123" הוסרו מהמחרוזת "hello123".

## צלילה עמוקה
- בהקשר היסטורי, פונקציה `strpbrk` מגיעה מהספריית הסטנדרטית של שפת תכנות C שהוכרזה בשנת 1972 על ידי דניס ריצ'י.
- ניתן להשתמש בפונקציות חלופות כמו `strstr`, או `strchr` אם הדרישה היא למצוא רק תו אחד.
- עבודה עם פונקציה `strpbrk` מחייבת זהירות, שכן כל שינוי במחרוזת המקורית או בתבנית ישפיע על התוצאה.

## ראה גם
- [מסמך הספרייה הסטנדרטית של C](https://he.wikipedia.org/wiki/%D7%A1%D7%A4%D7%A8%D7%99%D7%99%D7%AA_C)
- [strpbrk - C++ Reference](https://en.cppreference.com/w/c/string/byte/strpbrk)