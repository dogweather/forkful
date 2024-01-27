---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה עבודה עם CSV, ולמה זה חשוב? CSV (ערכים מופרדים פסיקים) הוא פורמט פשוט לאחסון נתונים טבלאיים. מתכנתים משתמשים בו כי הוא נפוץ, קל לקריאה וניתן לייבוא/ייצוא ממערכות רבות.

## How to:
נתחיל עם דוגמה של קריאה וכתיבה לקובץ CSV בשפת C:

```C
#include <stdio.h>
#include <stdlib.h>

// פונקציה לקריאת CSV
void readCSV(const char* filename) {
    FILE *file = fopen(filename, "r");
    char buffer[1024];

    if (file == NULL) {
        printf("לא ניתן לפתוח את הקובץ\n");
        return;
    }

    while (fgets(buffer, 1024, file)) {
        printf("%s", buffer);
    }

    fclose(file);
}

// פונקציה לכתיבה לCSV
void writeCSV(const char* filename) {
    FILE *file = fopen(filename, "w");

    if (file == NULL) {
        printf("לא ניתן לפתוח את הקובץ\n");
        return;
    }

    const char* data = "שם,גיל,עיר\nישראל ישראלי,30,תל אביב\nדנה כהן,25,ירושלים\n";
    fputs(data, file);

    fclose(file);
}

int main() {
    const char* filename = "דוגמה.csv";
    writeCSV(filename);
    readCSV(filename);
    return 0;
}
```

פלט:
```
שם,גיל,עיר
ישראל ישראלי,30,תל אביב
דנה כהן,25,ירושלים
```

## Deep Dive
CSV פעמים רבות הוא האמצעי הכי ישיר לחילופי נתונים בין תוכנות שונות, מאז המצאתו בשנות ה-70. קיימות חלופות כמו XML ו-Jason, אבל CSV ממשיך להיות נפוץ בזכות פשטותו. בקוד שלמעלה, בכוונה לא טיפלנו בצורה מורכבת בדאטה – יש לזכור שבמציאות יש צורך להתמודד גם עם בעיות כמו מידע חסר או נתונים לא תקינים.

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180), המגדיר את המבנה הסטנדרטי לקבצי CSV.
- [libcsv](http://sourceforge.net/projects/libcsv/), ספרייה עבור עיבוד קבצי CSV בשפת C.
- [GNU Datamash](https://www.gnu.org/software/datamash/), כלי שורת פקודה למניפולציה על נתונים טבלאיים, תומך גם בCSV.
