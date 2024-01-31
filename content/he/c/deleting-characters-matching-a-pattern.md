---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:41:57.757614-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
מחיקת תווים שתואמים תבנית היא כשמחפשים ומוחקים חלקים מספציפיים במחרוזת. תכנותים עושים את זה כדי לנקות נתונים, להסיר תווים לא רצויים, או לאמת פורמטים.

## How to:
קודים ב-C כדי למחוק תווים שתואמים תבנית:

```C
#include <stdio.h>
#include <string.h>

void deleteMatchingChars(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while (*src) {
        // החזק ב-src את הדמות הנוכחית
        const char *tempPattern = pattern;
        int isMatch = 0;

        // בדוק אם התו תואם לתבנית
        while (*tempPattern) {
            if (*src == *tempPattern++) {
                isMatch = 1;
                break;
            }
        }

        // אם אין תאמה, העתק הדמות
        if (!isMatch) {
            *dst++ = *src;
        }
        ++src;
    }
    *dst = '\0'; // קץ המחרוזת
}

int main() {
    char str[] = "Hello, World!";
    deleteMatchingChars(str, "lo");
    printf("תוצאה: %s\n", str); // תצפה לראות: He, Wr!d!
    return 0;
}
```

## Deep Dive:
מחיקת תווים היא לא רעיון חדש. מאז שמחרוזות הפכו לחלק משפות תכנות, הצורך לטפל בהן באופן פשוט ויעיל הופך לחשוב. אפשרויות אחרות כוללות ביטויים רגולריים (regex) ופונקציות סטנדרטיות של עיבוד מחרוזות. הדוגמה שלעיל היא יישום מודרני המשתמש בלולאות ובטיפול יעיל בזיכרון, מה שמאפשר הסרת תווים ממחרוזת מבלי לצרוך משאבים מיותרים.

## See Also:
- תיעוד של C Standard Library: https://en.cppreference.com/w/c/string
- פורסם על ביטויים רגולריים ב-C: http://man7.org/linux/man-pages/man3/regex.3.html
- טיפים לניקוי מחרוזות ב-C: https://stackoverflow.com/questions/tagged/c+string+cleaning
