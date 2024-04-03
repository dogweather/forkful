---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:28.043608-07:00
description: "\u05D1\u05E2\u05D5\u05DC\u05DD \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA\
  , \u05D4\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV\
  \ (\u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\
  \u05E4\u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\
  \u05D4 \u05DE\u05EA\u05D5\u05DA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05D5\u05DB\
  \u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD \u05E9\u05DC \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D1\u05E7\u05D5\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\
  \u05D8 \u05D4\u05DE\u05D0\u05D5\u05E8\u05D2\u05E0\u05D9\u05DD \u05DC\u05E4\u05D9\
  \ \u05E9\u05D5\u05E8\u05D5\u05EA, \u05DB\u05D0\u05E9\u05E8 \u05DB\u05DC \u05E9\u05D5\
  \u05E8\u05D4 \u05DE\u05D9\u05D9\u05E6\u05D2\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.167585-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05E2\u05D5\u05DC\u05DD \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA, \u05D4\
  \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 CSV (\u05E2\
  \u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\
  \u05E1\u05D9\u05E7) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E7\u05E8\u05D9\u05D0\u05D4\
  \ \u05DE\u05EA\u05D5\u05DA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05D5\u05DB\u05EA\
  \u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D4\u05DD \u05E9\u05DC \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D1\u05E7\u05D5\u05D1\u05E6\u05D9 \u05D8\u05E7\u05E1\u05D8\
  \ \u05D4\u05DE\u05D0\u05D5\u05E8\u05D2\u05E0\u05D9\u05DD \u05DC\u05E4\u05D9 \u05E9\
  \u05D5\u05E8\u05D5\u05EA, \u05DB\u05D0\u05E9\u05E8 \u05DB\u05DC \u05E9\u05D5\u05E8\
  \u05D4 \u05DE\u05D9\u05D9\u05E6\u05D2\u05EA \u05E8\u05E9\u05D5\u05DE\u05D4 \u05D5\
  \u05E9\u05D3\u05D5\u05EA \u05D4\u05E8\u05E9\u05D5\u05DE\u05D4 \u05DE\u05D5\u05E4\
  \u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D5\u05D1\u05E6\u05D9\
  \ CSV"
weight: 37
---

## מה ולמה?

בעולם התכנות, העבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת קריאה מתוך הקבצים וכתיבה אליהם של נתונים בקובצי טקסט המאורגנים לפי שורות, כאשר כל שורה מייצגת רשומה ושדות הרשומה מופרדים בפסיקים. מתכנתים מתעסקים בקבצי CSV בשל נוחות הייבוא/ייצוא של נתונים בין מערכות שונות, בזכות התמיכה הרחבה שלהם והפשטות שלהם לאחסון נתונים טבלאיים.

## איך לעשות:

### קריאה מקבצי CSV

לקרוא קובץ CSV ב-C, אנו משתמשים בפונקציות קלט/פלט קובצים סטנדרטיות יחד עם פונקציות עיבוד מחרוזות כדי לנתח כל שורה. למטה דוגמה בסיסית של קריאת קובץ CSV והדפסת שדותיו של כל שורה לקונסול.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
דוגמת `data.csv`:
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

דוגמת פלט:
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### כתיבה לקבצי CSV

באופן דומה, כתיבה לקובץ CSV כוללת שימוש ב-`fprintf` כדי לשמור נתונים בפורמט מופרד בפסיקים.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

תוכן דוגמת `output.csv`:
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## צלילה עמוקה

הפורמט CSV, למרות להיותו כנראה פשוט, מגיע עם העדפות משלו, כמו לטפל בפסיקים בתוך שדות ולסגור שדות במרכאות. הדוגמאות הראשוניות המוצגות לא לוקחות בחשבון סיבוכיות כאלו, וגם לא מטפלות בטעויות באופן קפדני.

בהיסטוריה, היידוע של קבצי CSV ב-C היה בעיקר ידני בזכות הטבע הרמה-נמוכה של השפה והחוסר באבסטרקציות גבוהות מובנות למשימות כאלו. הניהול הידני כולל פתיחת קבצים, קריאת שורות, פיצול מחרוזות, והמרת סוגי נתונים לפי הצורך.

כאשר נגיע לעבודה ישירה עם קבצי CSV ב-C היא מספקת ניסיון לימודי ערך על קלט/פלט של קבצים ועיבוד מחרוזות, מספר אלטרנטיבות מודרניות מבטיחות יעילות ותהליכים פחות שגויים. ספריות כמו `libcsv` ו-`csv-parser` מציעות פונקציות מקיפות לקריאה וכתיבה של קבצי CSV, כולל תמיכה בשדות במרכאות ומפרידים מותאמים אישית.

כחלופה, בעת עבודה בתוך אקוסיסטמים שתומכים בכך, אינטגרציה עם שפות או פלטפורמות שמספקות פונקציות גבוהות לעיבוד CSV (כמו Python עם ספריית ה-`pandas` שלה) יכולה להיות מסלול יותר פרודוקטיבי ליישומים הדורשים עיבוד CSV כבד. גישה זו בין-שפה מנצלת את יכולות הביצועים והתכנות של מערכות של C, תוך שימוש בנוחות משפות אחרות למשימות ספציפיות כמו עיבוד CSV.
