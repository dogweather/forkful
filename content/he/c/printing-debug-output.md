---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה זה ולמה? (What & Why?)

דפסת העוצמה של בדיקה היא לא זמנית שמשמשת לתיעוד ושיפור התנהלות התוכנה שלנו. מתכנתים משתמשים בדפסת בדיקה כדי לאפשר קוד שגיאה, ניתוח בעיות, ואופטימיזציה של מהירות.

## איך לעשות (How to):

בעזרת פונקציה 'printf()' של ספריית stdio.h אנו מציגים את הדפסת בדיקה. דוגמה לקוד שמציג 'Debug Mode On':

```C
#include <stdio.h>

int main() {
    #ifdef DEBUG
        printf("Debug Mode On\n");
    #endif
    return 0;
}
```

מה שמציג:

```C
Debug Mode On
```

## צלילה עמוקה (Deep Dive):

מאז תחילתה של שפת C, דפסת בדיקה התבצעה כחלק משלב הפיתוח. כיום אנו משתמשים בגרסאות שונות של printf() כדי להוסיף רמה נוספת של בקרה על התנהלות הדפסה. 

‏אלטרנטיבות נהילות ל-dprintf(3) תלויות באופן האווירה והצורך. לדוגמה, syslog() מתאים להתרמת יומנים באפליקציות שפועלות כשירותים.
    
פדפסת בדיקה מבצעת את העבודה שלה על ידי דפסת הודעות לפלט 'stderr', שאפשר להוסיף אליו כחלק מהקוד שלכם.

## ראה גם (See Also):

1. [תיעוד רשמי של C - printf](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)
2. [מאמר StackOverflow - מדוע אנו משתמשים ב-debug output?](https://stackoverflow.com/questions/888255/why-use-debug-output)
3. [ספר web.developer – תכנות בשפת C](https://www.w3resource.com/c-programming/c-debugging.php)