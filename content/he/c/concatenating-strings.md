---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
צירוף מחרוזות הוא פעולה שבה אנחנו מצרפים שתי מחרוזות או יותר ליחידה אחת. תכנתים מחברים מחרוזות כדי לארגן ולמעגל מידע בצורה ממוחשבת יותר.

## איך ל:
הנה דוגמה של כיצד לחבר שני מחרוזות ב-C:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char s1[10] = "שלום ";
   char s2[] = "לך";

   strcat(s1, s2);
   printf("%s\n", s1);

   return 0;
}
```

שיעור הפלט יהיה:
```
שלום לך
```

## צלילה מעמיקה
הוסף גרסת C הראשונה פונקציית `strcat`, פונקציה שמאפשרת לנו לחבר מחרוזות. אבל חשוב לזכור שהפונקציה לא מבצעת בדיקות גודל - כלומר, היא יכולה להוביל לגירסה אם המערך של המחרוזת המקורית שלא ברווח בצורה מספקת למחרוזת הדביקה.

בהינתן הפונקציות `strcat` ו `strncat`, האחרונה מקבלת ארגומנט נוסף המגביל את כמות התווים שנוספים מהמחרוזת שנשלחה. זה מסייע למנוע בעיות עם כתיבת מחוץ לגבולות המערך.

## ראה גם
1. מדריך C מלא: https://he.wikipedia.org/wiki/C_(שפת_תכנות)
2. תיעוד C: https://devdocs.io/c/
3. מדריך למחרוזות ב-C: https://www.learn-c.org/en/Strings