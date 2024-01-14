---
title:    "C: המרת תאריך למחרוזת"
keywords: ["C"]
---

{{< edit_this_page >}}

## למה
ישנם מספר סיבות מדוע ייתכן שתרצה להמיר תאריך למחרוזת בתוכנית יישומים שלך. אחת מהן היא להצגת התאריך בפורמט קריא יותר למשתמש. כמו כן, לפעמים ייתכן שתצטרך לשמור את התאריך המומר לגיבוי או לשליחה לשרת אחר. בכל מקרה, ייתכן שתצטרך להמיר את התאריך למחרוזת ולכן חשוב לדעת איך לעשות זאת.

## כיצד לעשות זאת
למימוש המרה זו ניתן להשתמש במגוון פונקציות שקיימות בשפת סי. הנה דוגמאות של שתי פונקציות מוכרות להמרת תאריך למחרוזת:

```C
#include <stdio.h>
#include <string.h>
#include <time.h>

int main()
{
    // תאריך נתון
    time_t rawtime;
    struct tm * timeinfo;
    char buffer[80];

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    // פורמט התאריך שנבחר
    strftime(buffer, 80, "התאריך הינו %d/%m/%Y", timeinfo);

    printf("%s\n",buffer);

    return 0;
}

```
פלט:
התאריך הינו 01/06/2020


```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // תאריך נתון
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    // פורמט התאריך שנבחר
    char* date_string = (char*)malloc(sizeof(char) * 11);
    sprintf(date_string, "%02d/%02d/%04d", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

    printf("התאריך הינו %s\n", date_string);

    return 0;
}

```

פלט:
התאריך הינו 01/06/2020


## חקירה מעמיקה
להמרת תאריך למחרוזת אינה משימה פשוטה וחישובית. פונקציות כמו `strftime()` משתמשות בתאריכים בפורמט `struct tm`, שיש בו מידע נוסף על התאריך כגון יום בשבוע, חודש ושנה. גם פונקציות כגון `gmtime()` ו `localtime()` משמשות במבנה דומה כדי להמיר את התאריך לפורמט התאריך הרגיל.

ניתן להשתמש גם ב