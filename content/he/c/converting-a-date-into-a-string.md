---
title:                "C: המרת תאריך למחרוזת"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מדוע
 Converting a date into a string is a useful skill to have in programming. It allows you to manipulate and display dates in a user-friendly format, making your programs more practical and visually appealing. Whether you need to display dates in a specific format or perform calculations using date and time, being able to convert them into strings is an essential skill to have.

## איך לעשות זאת
כדי להמיר תאריכים למחרוזות בקוד C, ישנם מספר דרכים. אחת הדרכים הנפוצות היא על ידי שימוש בפונקציית `strftime()`, המאפשרת לנו להפוך תאריכים למחרוזות בפורמט שאנחנו רוצים. ניצור תוכנית קטנה שתדגים איך זה עובד:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL); // מקבל את התאריך והשעה הנוכחיים
  char date_string[50];

  strftime(date_string, 50, "%d/%m/%Y", localtime(&now)); 

  printf("Today's date is: %s\n", date_string); // הדפסת התאריך הנוכחי כמחרוזת בפורמט ״dd/mm/yyyy״
  return 0;
}
```

פלט:
```
Today's date is: 01/01/2021
```

כאן אנחנו משתמשים בפונקציית `strftime()` כדי להמיר את התאריך הנוכחי למחרוזת בפורמט ״dd/mm/yyyy״ ולהדפיס אותו.

וכדי לעשות את זה בפורמט מסוגרת, נוסיף עוד שורת קוד:

```C
#include <string.h>

int main() {
  // קוד קודם
  strftime(date_string, 50, "Today's date is: (%d/%m/%Y)", localtime(&now));

  printf("%s", date_string);
  return 0;
}
```
 
פלט:
```
Today's date is: (01/01/2021)
```

כפי שאתם רואים, ישנן מגוון רחב של אפשרויות להמרת תאריכים למחרוזת, והיישומים שלה הם מגוונים וחשובים.

## מנתחים עמוק
ההמרה של תאריכים למחרוזת היא עניין של פונקציות מובנות שמאפשרות לנו לצור את המחרוזת המבוקשת בקלות. הפונקציות המשמשות להמרת תאריכים למחרוזות נמצאות בתכניות הספריה `time.h` ומציעות מגוון רחב של אפשרויות להצגה ופורמ