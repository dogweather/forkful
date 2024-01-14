---
title:                "C: ממירים תאריך למחרוזת"
simple_title:         "ממירים תאריך למחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

על מה אנחנו מדברים?

בכתיבת קוד עבור תכנית מחשב, ייתכן שתתקלו בצורך להמיר תאריך מסוים למחרוזת. המטרה היא להציג את התאריך בפורמט שאנשים יכולים לקרוא ולהבין בקלות, כגון "יום-חודש-שנה" או "חודש-יום-שנה".

איך לעשות את זה?

כדי לבצע את המימוש הזה בשפת סי, ניתן להשתמש בפונקציית "strftime". להלן דוגמה של שימוש בפונקציה להמרת תאריך למחרוזת:

```C
#include <time.h>
#include <stdio.h>
int main() {
  time_t now = time(NULL);
  struct tm *timeinfo = localtime(&now);
  
  char buffer[80];
  strftime(buffer, 80, "%d-%m-%Y", timeinfo);
  printf("התאריך היום הוא: %s\n", buffer);
  return 0;
}
```

התאריך היום הוא: 01-01-2020

דיבוג:
הפונקציה "strftime" מאפשרת לנו לציין את הפורמט של המחרוזת שנרצה לקבל כתוצאה. ניתן להשתמש בתווים שונים לכתוב את התאריך בפורמט הרצוי, כגון %d ליום, %B לחודש, ו-%Y לשנה.

עוד דברים לדעת:
הפונקציה "strftime" נמצאת בספריית הסטנדרטית המתאימה לשפת סי – "time.h". לכן, יש לוודא שהקובץ המכיל את הקוד שלנו מכיל גם את הג'ירה "#include <time.h>". כמו כן, יש לוודא כי נשתמש במשתנה "now" שמכיל את הזמן הנוכחי ובמשתנה "timeinfo" שיכיל את פרטי התאריך שנרצה להמיר למחרוזת.

ראו גם:
- [מדריך על strftime ב-GeeksforGeeks](https://www.geeksforgeeks.org/strftime-function-in-c/)
- [תיעוד של הפונקציה strftime באתר הרשמי של Microsoft](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/strftime-wcsftime-strftime-l-wcsftime-l?view=vs-2019)