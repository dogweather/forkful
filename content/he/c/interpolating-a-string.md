---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:50:43.794160-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מילוי מחרוזת הוא שילוב נתונים דינמיים עם טקסט קבוע. תכניתנים משתמשים בזה כדי ליצור מחרוזות דינמיות לצרכי תצוגה, לוגים, וממשק משתמש.

## איך לעשות:
ב-C, אין תמיכה ישירה במילוי מחרוזת כמו בשפות אחרות. אבל עוד אפשר להשיג את התוצאה הזו עם `sprintf`, `snprintf`, או משפחת הפונקציות `printf`. דוגמה:

```c
#include <stdio.h>

int main() {
    char name[] = "שלמה";
    int age = 28;
    char message[50];

    snprintf(message, sizeof(message), "שלום, קוראים לי %s ואני בן %d.", name, age);

    printf("%s\n", message);
    return 0;
}
```

פלט:
```
שלום, קוראים לי שלמה ואני בן 28.
```

## עיון מעמיק
מילוי מחרוזת ב-C מורכב יותר מאשר בשפות כמו Python או JavaScript. ההיסטוריה של זה חוזרת למקורות של השפה, שבה כל דבר נבנה לקרבה למערכת ההפעלה והחומרה. אלטרנטיבות כוללות פונקציות כמו `sprintf` ו`snprintf`, אבל יש להיזהר מבעיות בטיחות וכדאי להשתמש ב`snprintf` כדי למנוע גודש זיכרון. לפרטים נוספים על יישום, תמיד בדוק את גודל הבאפר שלך לפני השימוש בפונקציות אלו.

## ראה גם
- [C++ : ראה גם את std::format (C+20)](https://en.cppreference.com/w/cpp/utility/format/format)
- [Python : קריאה על f-strings למילוי פשוט יותר של מחרוזות](https://docs.python.org/3/tutorial/inputoutput.html#tut-f-strings)
- [GNU libc manual on printf - לעומק על`printf`](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)
- [C Secure Coding - למידע על בטיחות כאשר משתמשים בפונקציות מילוי מחרוזת](https://wiki.sei.cmu.edu/confluence/display/c/STR31-C.+Guarantee+that+storage+for+strings+has+sufficient+space+for+character+data+and+the+null+terminator)
