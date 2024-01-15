---
title:                "עבודה עם קבצי CSV"
html_title:           "C: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

 המון מפתחים משתמשים בקבצי CSV כי הם דרך נוחה ופשוטה לאחסן ולעבד מידע. תוכלו להשתמש ב-CSV לכל מטרה משתופת לסדר נתונים כמו נתוני משתמשים או נתוני עסקיים.

## איך לעבוד עם CSV בשפת C

 כדי להתחיל לעבוד עם קבצי CSV בשפת C תצטרכו לחבר את הספרייה המתאימה `stdio.h` ולהשתמש בפונקציות `fopen()` לפתיחת הקובץ ו- `fprintf()` לכתיבת הנתונים. להלן דוגמא לכתיבת נתונים לקובץ CSV:

```C
#include <stdio.h>

int main()
{
  FILE* fp = fopen("data.csv", "w"); // פתיחת הקובץ לכתיבה

  // כתיבת כותרת העמודות
  fprintf(fp, "Name, Age, City\n");

  // כתיבת נתונים
  fprintf(fp, "John, 25, New York\n");
  fprintf(fp, "Emma, 30, London\n");
  
  // סגירת הקובץ
  fclose(fp);

  return 0;
}
```

הפלט של קוד זה יהיה קובץ CSV עם שורה ראשונה המכילה את הכותרות ושורות נוספות המכילות את הנתונים המתאימים. לדוגמה:

```
Name, Age, City
John, 25, New York
Emma, 30, London
```

## צלילה עמוקה

מאחר וקבצי CSV יכולים להיות מאוד רווחיים ומכילים נתונים מורכבים, חשוב להכיר כמה דברים כדי לעבוד איתם בצורה נכונה. ראשית, כדאי להשתמש בכותרות לכל עמודה כדי להפעיל את הנתונים מסודרים בצורה נכונה. כמו כן, כדאי לוודא שהנתונים מאוחסנים בצורה מתאימה כדי למנוע בעיות בזמן הכתיבה או קריאה של הנתונים.

## ראו גם

- תיעוד של פונקציות ה-SQL המשמשות לעבוד עם קבצי CSV בשפת C: https://www.sqlite.org/c3ref/funclist.html
- מדריך מ