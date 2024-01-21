---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:46:04.378351-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא תהליך שבו אנו מייצרים מחרוזת חדשה מתוך מחרוזת קיימת. תכניתנים מבצעים זאת כדי לעבד רק חלק מן המידע, לוודא תקינות של קלט או לאסוף נתונים ספציפיים.

## איך לעשות:
```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "שלום, עולם!"; // המחרוזת המקורית
    char substr[5]; // כאן נשמור את תת-המחרוזת

    // חילוץ תת-מחרוזת: ארבעה תווים החל מתו 7 במחרוזת (תו 0 הוא התו הראשון)
    strncpy(substr, &text[6], 4);
    substr[4] = '\0'; // אל תשכחו לחתום את תת-המחרוזת עם תו סיום

    printf("תת-מחרוזת: %s\n", substr); // הדפסת תת-המחרוזת

    return 0;
}
```
תוצאה: `תת-מחרוזת: עולם`

## צלילה עמוקה:
בימים הראשונים של התכנות, ניהול מחרוזות היה מורכב ומוגבל יותר. חילוץ תת-מחרוזות היום הוא משימה פשוטה בהרבה בזכות פונקציות סטנדרטיות כמו `strncpy`. חלופות כוללות שימוש בפונקציות מותאמות אישית ובספריות נוספות שמקלות על אלגוריתמים מתוחכמים יותר של מניפולציות מחרוזות.

השימוש ב-`strncpy` דורש זהירות: אם המקור ארוך מדי, היעד לא ייחתם באופן אוטומטי ב-`\0`. תמיד וודאו שיש מקום לתו הסיום!

## ראו גם:
- [C String Manipulation](https://www.tutorialspoint.com/cprogramming/c_strings.htm) - טיפול במחרוזות ב-C, אתר TutorialsPoint.
- [C String Reference](https://en.cppreference.com/w/c/string/byte) - העמוד להתייחסות לפונקציות מחרוזת ב-C, אתר CPP Reference.
- [Strncpy Man Page](https://linux.die.net/man/3/strncpy) - דף המדריך (man page) לפונקציה `strncpy`.