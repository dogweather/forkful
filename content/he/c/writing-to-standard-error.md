---
title:                "כתיבה לשגיאת התקן"
html_title:           "C: כתיבה לשגיאת התקן"
simple_title:         "כתיבה לשגיאת התקן"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

מה זה ולמה?

כתיבה לסטנדרט צעדר (standard error) הוא דרך להדפיס שגיאות ומידע חשוב למשתמשים ממידע הדפסה (standard output). זה מאפשר למתכנתים למצוא את הטעויות בפיתוח ולספק מידע נוסף למשתמשים שמפעילים את התוכנית. 

איך לעשות זאת?

```
#include <stdio.h>
int main() {
    fprintf(stderr, "זהו מסר שכתוב אל הסטנדרט צעדר. \n");
    return 0;
}
```
    
**הפלט של התוכנית:**

```
זהו מסר שכתוב אל הסטנדרט צעדר.
```

טיפ: השתמשו ב`fprintf()` עם הפרמטר `stderr` כדי לכתוב אל סטנדרט השגיאות. 

כדי להדפיס פרמטרים משתנים, ניתן להשתמש באותו הדרך כמו בהדפסה לסטנדרט היציאה:

```
#include <stdio.h>
int main() {
    int num = 10;
    fprintf(stderr, "מספר %d הוא מספר ראשוני. \n", num);
    return 0;
}
```

**הפלט של התוכנית:**

```
מספר 10 הוא מספר ראשוני.
```

**עוד על כתיבה לסטנדרט צעדר:**

גרסת C הנוכחית משתמשת בסטנדרטים קבועים כדי להסביר כיצד על מנת לכתוב לסטנדרט צעדר. בעבר, מומלץ היה להשתמש בפונקציות על ידי שם, כמו `write()`, במקום להשתמש ב`fprintf()` שבאופן סופי מפעיל את `write()`. אבל, בשל השימוש הנפוץ יותר של `fprintf()`, זה די בקטע קוד ומכווץ את הקוד.

ראו גם:

- מדריך לכתיבה מושכלת לסטנדרט צעדר: https://wiki.sei.cmu.edu/confluence/display/c/SIG07-C.+Use+reasonable,+secure+error-handling+idioms
- מדריך למפתחים שנמצאים בעולם המתקדם: https://github.com/gomiller/standarderrors
- "C שלכם עוד חסר כאן | C WIKI Cookbook": https://en.wikibooks.org/wiki/C_Programming/C_Reference/stdio_h/fprintf