---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני הוא תהליך של הקמת קובץ מתוך א מרכז עיבוד נתונים שמשמש לאחסון נתונים באופן זמני. המתכנתים עושים זאת מרוב שיקולים: אילוץ של מערכת ההפעלה, עיבוד מראש של קבצים גדולים, או אפשרים לבצע עיבוד רב מקבילי.

## איך ליצור:
הנה דוגמא חינם של קוד בשפת C ליצירת קובץ זמני:

```C
#include <stdio.h>

int main() {
    char temp_filename[] = "/tmp/tempfileXXXXXX";
    int file_descriptor = mkstemp(temp_filename);

    if(file_descriptor == -1) {
        printf("Cannot create temporary file, do you have proper access rights?\n");
        return 1;
    }

    // Now the temporary file is open. You can write/read/erase it.
    ...
}
```

## הצצה לעומק:
הייתה בעבר תכנית C standard ששימשה ליצירת קבצים זמניים `tmpfile()`, אך חשיפה של חולשהית בטחון תפעולית גרמה לבניית `mkstemp()`. אפשרות חלופית היא שימוש ב-POSIX `mkstemp()`, שאף היא מספקת  יכולת החלפה של שם הקובץ במזהה מספרי מרחק משמש.
היישום מאמן גלם תחילה את המחרוזת המועתקת `XXXXXX` במספרים עד שמספר הקובץ החדש לא יהיה מתנגש עם שם קובץ קיים.

## ראה גם:
הדף של האינטרנט של מסמכי POSIX [mkstemp()](http://man7.org/linux/man-pages/man3/mkstemp.3.html)
נספח המילון של The GNU C Library, תחת ["Temporary Files"](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html) במידע נוסף על קבצים זמניים.