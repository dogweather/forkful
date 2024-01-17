---
title:                "המרת תאריך למחרוזת"
html_title:           "C: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?

המרת תאריך למחרוזת היא תהליך שבו מתאריך מסוים נמצא בפורמט של מחרוזת. תהליך זה חשוב למתכנתים מכיוון שהוא מאפשר להציג תאריך בצורה מותאמת לכל פלטפורמה או דיבור, מבלי להשתמש בתאריך מקורי.

# איך לעשות זאת:

```C
#include <stdio.h>
#include <time.h>

int main()
{
   time_t now;
   time(&now);
   char buffer[80];
   strftime(buffer, 80, "Today is %A, %B %d.", localtime(&now));
   printf("Date: %s", buffer);
   return 0;
}
```
**Output:** Date: Today is Wednesday, September 29.

# הנכים לעומק:

תהליך המרה של תאריך למחרוזת קיים מאז ילדות המחשבים והשתמש בהיסטוריית התאריכים הצביונית לחישוב התאריך היהודי והמולד.

אחת האלטרנטיבות הנפוצות יותר לתהליך המרה הזה היא לשמור את התאריך במבנה נתון מתאים כמו משתנה מסוג struct או להשתמש בספריית תוצרת צד ג'ייסון.

פירוט יותר על תהליך המרה כולל דוגמאות ומידע נוסף ניתן למצוא במקורות המקושרים למטה.

# ראה גם:

- [מדריך של MSDN על המרת תאריך למחרוזת בשפת C](https://docs.microsoft.com/en-us/cpp/standard-library/strftime-wcsftime-strftime-l-wcsftime-l)
- [מדריך של GeeksforGeeks על המרת תאריך למחרוזת בשפת C](https://www.geeksforgeeks.org/converting-string-different-date-formats-using-strptime/?ref=rp)
- [ספריית ממשק תרגום של gettext לשפת C](https://www.gnu.org/software/gettext/)