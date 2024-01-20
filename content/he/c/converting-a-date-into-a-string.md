---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? (What & Why?)

המרת תאריך למחרוזת היא תהליך של שינוי התאריך מפורמט של משתנה מסוג תאריך למשתנה מסוג מחרוזת. מתכנתים עושים את זה כדי להציג או לשמור את התאריך בצורה קריאה לאנוש.

## איך: (How to:)

```C
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm * timeinfo;

    time (&rawtime);
    timeinfo = localtime (&rawtime);

    strftime (buffer,80,"אנו כעת בשעה %I:%M%p והתאריך הוא %A, %B %d, %Y.",timeinfo);
    puts (buffer);

    return 0;
}
```

אתה יכול לצפות לראות פלט מהסוג של: "אנו כעת בשעה 07:26PM והתאריך הוא יום שני, נובמבר 22, 2021."

## צלילה עמוקה (Deep Dive)

1. **קונטקסט היסטורי:** בעבר, מתכנתים היו נאלצים לכתוב שורות רבות של קוד על מנת להמיר תאריך למחרוזת. אך עם הזמן, שפות תכנות חדישות כמו C התפתחו וסיפקו פונקציות מובנות שמקלות על התהליך.
   
2. **אלטרנטיבות:** אתה יכול גם להשתמש בפונקציה sprintf, אך המלצתנו היא להשתמש בפונקציה strftime שכוללת יותר אפשרויות עיצוב.
   
3. **פרטי התממשק:** נציב הפונקציה strftime מאפשר לך לעצב את המחרוזת ערך באופן שאתה מעוניין. %.A מקביל לשם היום, %.B לשם החודש, %.d ליום בחודש, %.Y לשנה.

## ראה גם (See Also)

1. [תיעוד של strftime](https://www.cplusplus.com/reference/ctime/strftime/)
3. [C library function - strftime()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)