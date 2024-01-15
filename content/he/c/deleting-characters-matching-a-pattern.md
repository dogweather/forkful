---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "C: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מדוע

המחיקה של תווים המתאימים לתבנית יכולה להיות מועילה כאשר מעוניינים לעבוד בצורה יעילה עם מחרוזות מסוימות. למשל, זה יכול לסייע בניקוי נתונים או בתיקון שגיאות בקוד. 

## איך לעשות זאת

אנחנו נראה כמה דוגמאות של קוד כדי להדגים איך למחוק תווים עם תבנית מסוימת בשפת C. כל קוד יהיה בתוך בלוקיי "```C ... ```". 

1. התווים שהתוכנית מחפשת בדוגמה הזו הם פסיקים. אנחנו נשתמש בפקודת "strchr" כדי למצוא את התו הראשון במחרוזת שמתאים לפסיק ולמחוק אותו מהמחרוזת.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "ממש פסיקים, פסיקים הרבה!";
    char to_delete = ',';
    char *result = NULL;

    while ((result = strchr(str, to_delete))) {
        memmove(result, result + 1, strlen(result));
    }
    printf("%s\n", str);
    
    return 0;
}
```

2. נציג עוד דוגמה שמשתמשת בפקודת "strpbrk" כדי למצוא את התו הראשון שמתאים לסט תווים נתון ולמחוק אותו מהמחרוזת. בקוד זה, התווים המחפשים הם מספרים מן התווים 0-9.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "4, 8, 15, 16, 23, 42";
    char *to_delete = "0123456789";
    char *result = NULL;

    while ((result = strpbrk(str, to_delete))) {
        memmove(result, result + 1, strlen(result));
    }
    printf("%s\n", str);
    
    return 0;
}
```

הפלט לשתי הדוגמאות יהיה "ממש פסיקים רבים!" ו"אם לא נתקעת בשביל לשתות את הפסיכיאטר טולטק, כל מבנה יודע איך להפיל" בהתאמה. 

## להעמיק

החיפוש והמחיקה של תווים מתפרקת לשני חלקים בשפת C - שימוש בפקודות מתאימות כדי למצוא את התו או התווים שרוצים למחוק