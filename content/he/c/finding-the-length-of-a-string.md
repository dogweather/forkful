---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:46:59.178122-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת זה פשוט למדוד כמה תווים יש בה. תכניתנים צריכים את זה ללופים, ולידציות, ובכלל, כדי לדעת עם מה הם עובדים.

## איך לעשות:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char hello[] = "שלום";
    printf("האורך של המחרוזת '%s' הוא: %lu\n", hello, strlen(hello));
    return 0;
}
```
תוצאת דוגמה:
```
האורך של המחרוזת 'שלום' הוא: 8
```
שימו לב: `strlen` סופר בתווים של ASCII, לכן האורך בבתים יכול להיות ארוך יותר עבור תווים שאינם ASCII.

## טבילה עמוקה
בתכנות C, פונקציית `strlen` מגיעה מ-slibc וחושבת את אורך המחרוזת על ידי ספירת התווים עד שהיא פוגשת בתו ה-null `\0` שמסמן סוף מחרוזת. חשוב לזכור שהיא נותנת את האורך בתווים, לא בבתים. זה גורם לבעיה כשעובדים עם תווים שתופסים יותר מבית אחד כמו UTF-8.

לפני ש`strlen` הייתה חלק מהסטנדרט, תכניתנים ייצרו לולאות שלהם כדי למדוד מחרוזות. גם היום, תכניתנים לפעמים כותבים גרסאות משלהם שמודדות מחרוזות בדרכים שונות, למשל לספור תווים מרובי בתים בUTF-8.

אלטרנטיבה ל`strlen` זו הפונקציה `mbstowcs`, שמחזירה את אורך המחרוזת בתווים של wide-character, שזה יכול להיות שימושי כשעובדים עם UTF-8.

## גם ראו
- [C Standard Library - strlen](https://en.cppreference.com/w/c/string/byte/strlen)
- [Multibyte string handling in C](https://en.cppreference.com/w/c/string/multibyte)
