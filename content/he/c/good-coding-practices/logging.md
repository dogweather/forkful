---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:55.837485-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D1\u05E9\u05E4\u05EA C \u05DB\u05D5\
  \u05DC\u05DC\u05EA \u05EA\u05D9\u05E2\u05D5\u05D3 \u05D0\u05EA \u05D6\u05E8\u05D9\
  \u05DE\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D5\u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD \u05D1\u05D5\u05DC\u05D8\u05D9\u05DD \u05D1\u05DE\u05D4\
  \u05DC\u05DA \u05D6\u05DE\u05DF \u05D4\u05E8\u05D9\u05E6\u05D4 \u05E9\u05DC\u05D4\
  , \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05D1\u05D3\u05D9\u05E7\u05D4\
  \ \u05DE\u05D5\u05D7\u05E9\u05D9\u05EA \u05E9\u05DC \u05D4\u05EA\u05E0\u05D4\u05D2\
  \u05D5\u05EA\u05D4 \u05D5\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05D4. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05DC\u05D5\u05D2\u05D9\u05DD \u05DC\u05E6\u05D5\u05E8\u05DA\u2026"
lastmod: '2024-03-13T22:44:40.137974-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D1\u05E9\u05E4\u05EA C \u05DB\u05D5\u05DC\
  \u05DC\u05EA \u05EA\u05D9\u05E2\u05D5\u05D3 \u05D0\u05EA \u05D6\u05E8\u05D9\u05DE\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D5\u05D0\u05D9\u05E8\u05D5\
  \u05E2\u05D9\u05DD \u05D1\u05D5\u05DC\u05D8\u05D9\u05DD \u05D1\u05DE\u05D4\u05DC\
  \u05DA \u05D6\u05DE\u05DF \u05D4\u05E8\u05D9\u05E6\u05D4 \u05E9\u05DC\u05D4, \u05DE\
  \u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DE\
  \u05D5\u05D7\u05E9\u05D9\u05EA \u05E9\u05DC \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\
  \u05EA\u05D4 \u05D5\u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05D4. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05DC\
  \u05D5\u05D2\u05D9\u05DD \u05DC\u05E6\u05D5\u05E8\u05DA\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

לוגים בשפת C כוללת תיעוד את זרימת התוכנית ואירועים בולטים במהלך זמן הריצה שלה, מה שמאפשר בדיקה מוחשית של התנהגותה וביצועיה. מתכנתים משתמשים בלוגים לצורך איתור באגים, ניטור בריאות התוכנה ושמירה על אבטחת המערכת.

## איך לעשות:

ב-C, ניתן לבצע רישום לוגים באמצעות פעולות קובץ בסיסיות או באמצעות ספריות מתוחכמות יותר. לצורך הפשטות, נתחיל עם ספריית ה-I/O הסטנדרטית. הקטעים הבאים מציגים יישום בסיסי של לוגים.

לרישום הודעות פשוטות:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // פתיחת קובץ הלוג במצב הוספה
    
    if (logFile == NULL) {
        perror("שגיאה בפתיחת קובץ הלוג.");
        return -1;
    }
    
    fprintf(logFile, "התוכנית מתחילה.\n");
    
    // הלוגיקה של היישום שלך כאן
    
    fprintf(logFile, "היישום הסתיים בהצלחה.\n");
    fclose(logFile);
    
    return 0;
}
```

פלט ב-`application.log`:

```
התוכנית מתחילה.
היישום הסתיים בהצלחה.
```

לכלול לוגים מפורטים יותר עם חותמות זמן ורמות לוג:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // הסרת תו שורה חדשה
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("שגיאה בפתיחת קובץ הלוג.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "היישום מתחיל");
    // הלוגיקה של היישום שלך כאן
    logMessage(logFile, "ERROR", "דוגמא לשגיאה");
    
    fclose(logFile);
    
    return 0;
}
```

פלט ב-`detailed.log`:

```
[Thu Mar 10 14:32:01 2023] INFO - היישום מתחיל
[Thu Mar 10 14:32:02 2023] ERROR - דוגמא לשגיאה
```

## ניתוח מעמיק

רישום לוגים ב-C, כפי שהודגם, נשען על פעולות קובץ פשוטות, אשר אף על פי שהן יעילות אינן כה חזקות או גמישות כמו האפשרויות לרישום לוגים בשפות אחרות, כמו מודול `logging` ב-Python או `Log4j` ב-Java. לצורך יכולות רישום לוגים מתקדמות יותר ב-C, מפתחים לעיתים קרובות פונים לספריות כמו `syslog` במערכות הדמויות Unix, אשר מספקות ניהול לוגים ברמת המערכת, או לספריות צד שלישי כמו `log4c`.

באופן היסטורי, רישום לוגים היה חלק בלתי נפרד מתיכנות, עוד מימי התכנות הראשונים שבהם נעשה שימוש בהדפסות פיזיות כדי לעקוב ולהבין את זרימת התוכנית והשגיאות. עם התפתחות המערכות, רישום הלוגים התפתח להיות יותר מתוחכם, כולל תמיכה ברמות חומרה שונות של הלוגים, סיבוב לוגים, ורישום לוגים אסינכרוני.

למרות שספריית הסטנדרט של C מספקת את הכלים הבסיסיים להטמעת רישום לוגים, הגבלותיה תכופות מובילות ליצירת מסגרות לוגים מותאמות אישית או לאימוץ ספריות חיצוניות לפתרונות לוגים עשירים וגמישים יותר בתכונות. למרות ההגבלות האלה, הבנה ויישום של רישום לוגים בסיסי ב-C הן חיוניות לאיתור באגים ותחזוקת התוכנה, במיוחד בסביבות שבהן יש למזער תלות בתלויות חיצוניות.
