---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-stderr מאפשרת לתכנות לדווח על שגיאות ובעיות בזמן הרצה. זה חשוב כדי לפרק את הפלט הרגיל מהודעות השגיאה.

## איך לעשות:
קוד מדוגם ופלט:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "שגיאה: קרתה בעיה!\n");
    return 0;
}
```
פלט תכנית:
```
שגיאה: קרתה בעיה!
```

## עומק הצלילה
בזמנים של לוחות פאנץ', פלט נפרד לשגיאות לא היה קיים. התפתחות הטרמינלים ומערכות ההפעלה הובילה לניתוק שלם עם stderr. חלופות כוללות כתיבה לקובץ יומן או שימוש ב-macros כמו perror. אופן היישום: stderr היא גלובלית ומוגדרת כ־FILE* ב-stdio.h.

## ראה גם
מסמכים ומדריכים נוספים:
- תיעוד GNU libc על stderr: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- מאמר ויקיפדיה על זרימת קלט/פלט ב-C: https://en.wikipedia.org/wiki/C_file_input/output
