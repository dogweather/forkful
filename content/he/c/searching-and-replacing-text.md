---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:57:56.444200-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט היא תכנות בסיסי שבו אנחנו מחפשים תווים או מחרוזות בטקסט ומחליפים אותם באחרים. תכניתנים עושים את זה לתיקון מהיר של שגיאות, עידכון נתונים או שינוי פורמטים.

## איך לעשות:
ב-C, אנחנו משתמשים בפונקציות כמו `strstr()` לחיפוש מחרוזת בתוך מחרוזת אחרת ו`strcpy()` או `strncpy()` להחלפה. דוגמא פשוטה:

```c
#include <stdio.h>
#include <string.h>

void search_and_replace(char *source, const char *search, const char *replace) {
    char buffer[1024];
    char *p;

    if ((p = strstr(source, search)) == NULL) {
        printf("המחרוזת לא נמצאה.\n");
        return;
    }
    
    strncpy(buffer, source, p - source);
    buffer[p - source] = '\0';

    sprintf(buffer+(p - source), "%s%s", replace, p + strlen(search));
    strcpy(source, buffer);

    printf("התוצאה החדשה: %s\n", source);
}

int main() {
    char text[] = "שלום עולם, זה סתם דוגמא!";
    search_and_replace(text, "סתם", "מדהימה");
    return 0;
}
```

תצאה:
```
התוצאה החדשה: שלום עולם, זה מדהימה דוגמא!
```

## צלילה לעומק
מאז שנוצר ה-C בשנות ה-70, חיפוש והחלפת טקסט הפך לכלי רב עוצמה. פונקציות כמו `strstr()` ו`strchr()` עוזרות בחיפוש תווים או מחרוזות. עוד דרכים כוללות רגולר אקספרשנס ואת כלי קו הפקודה `sed` בלינוקס. במימוש, תקפידו לשים לב ל-overflow ושימוש בזיכרון בטוח, במיוחד כאשר מתעסקים עם מחרוזות ב-C.

## ראה גם
- התיעוד הרשמי של C99, C11, ו-C18 עבור פונקציות מחרוזות: https://en.cppreference.com/w/c/string/byte
- למידע על רגולר אקספרשנס: https://regexone.com/
- `sed` - עורך טקסט שורת פקודה ל-UNIX: https://www.gnu.org/software/sed/manual/sed.html
