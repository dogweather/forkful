---
title:                "עבודה עם קובצי CSV"
aliases: - /he/c/working-with-csv.md
date:                  2024-02-03T18:12:28.043608-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם קובצי CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
