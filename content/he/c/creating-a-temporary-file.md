---
title:                "יצירת קובץ זמני"
html_title:           "C: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##למה
למה כדאי לכם ליצור קובץ זמני? אתם יכולים להשתמש בקבצים זמניים בתהליכי לעבודה, לשמור מידע או לבדיקת פונקציות בקוד.

##כיצד
הנה כמה דוגמאות ליצירת קובץ זמני בשפת סי באמצעות הפונקציה mktemp:

```C
#include <stdio.h>
 
int main () {
   FILE *fp;

   fp = fopen("/tmp/tempfile", "w+");
   fputs("Temporary File", fp);
   
   printf("Temporary file created successfully.");
   
   fclose(fp);
   
   return(0);
}
```
פלט:
```
Temporary file created successfully.
```
בדוגמה זו, אנו יוצרים קובץ זמני במיקום /tmp/tempfile ומכניסים לו טקסט.

הנה דוגמא נוספת עם השתמשות במצב "קריאה" במקום "כתיבה" לקריאה מקובץ קיים:

```C
#include <stdio.h>
 
int main () {
   FILE *fp;

   fp = fopen("/tmp/tempfile", "r");
   
   if (fp == NULL) {
      printf("Temporary file not found.");
      return 1;
   }
   
   char buffer[255];
   
   while(fgets(buffer, 255, fp) != NULL) {
      printf("%s", buffer);
   }
   
   fclose(fp);
   
   return(0);
}
```
פלט:
```
Temporary File
```
הנה דוגמא נוספת עם השתמשות במצב "הוספה" לכתיבת נתונים לקובץ זמני:

```C
#include <stdio.h>
 
int main () {
   FILE *fp;

   fp = fopen("/tmp/tempfile", "a");
   fputs(" New Data", fp);
   
   printf("New data added successfully.");
   
   fclose(fp);
   
   return(0);
}
```
פלט:
```
New data added successfully.
```
כעת, אם נפעיל את קידומי המצב בשורה ה 6 בקובץ המקורי, נוכל להיות בטוחים שהקובץ זמין לשמירת נתונים חדשים בכל פעם שהתוכנית תתבצע.

##עומק
אחת מהפונקציות הנפוצות ליצירת קובץ זמני היא mkstemp. פונקציה זו מקבלת שני משתנים - מחרוזת המציינת את קידומי הקובץ וקידומת הקובץ, ומחזירה מצביע לקובץ זמני חדש שנוצר. בניגוד לפונקציה mktemp, אין צורך לדאוג לשמירת קובץ זמני