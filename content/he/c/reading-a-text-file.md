---
title:                "C: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה
קריאת קובץ טקסט נחשבת לפעולה חשובה בתכנות בשפת C. היא מאפשרת למשתמש לקרוא נתונים מתוך קובץ ולהשתמש בהם בתוכנית שלו על מנת למקד את העיבוד וביצוע אופציות אחרות.

## כיצד לבצע קריאת קובץ טקסט בשפת C
כדי לבצע קריאת קובץ טקסט בשפת C, ניתן להשתמש בפונקציית fopen כדי לפתוח את הקובץ ולקרוא את הנתונים שלו לתוך משתנה מתאים. למשל:

```C
FILE *fp;
char buffer[100];
int num;

fp = fopen("example.txt", "r"); // פתיחת קובץ קיים לקריאה
if (fp == NULL) {
  printf("Error opening file!"); // הודעת שגיאה אם הפתיחה נכשלת
  return 1;
}

num = fscanf(fp, "%s", buffer); // קריאת מידע מהקובץ לתוך המשתנה
if (num > 0) {
  printf("Data from file: %s", buffer); // הדפסת המידע שנקרא מהקובץ
}

fclose(fp); // סגירת הקובץ
```
**פלט:**
```
Data from file: Hello, World!
```

כדי לקרוא את הנתונים בקובץ לפי שורות, ניתן להשתמש בפונקציית fgets כדי לקרוא את כל התוכן של השורה לתוך משתנה מסוג string. למשל:

```C
FILE *fp;
char buffer[100];

fp = fopen("example.txt", "r"); // פתיחת קובץ קיים לקריאה
if (fp == NULL) {
  printf("Error opening file!"); // הודעת שגיאה אם הפתיחה נכשלת
  return 1;
}

while (fgets(buffer, 100, fp) != NULL) { // קריאת תוכן כל שורה והצגתה
  printf("%s", buffer);
}

fclose(fp); // סגירת הקובץ
```
**פלט:**
```
Hello, World!
This is a sample text file.
```

## מעמיקים יותר בקריאת קובץ טקסט
קריאת קובץ טקסט בשפת C יכולה להיות מסובכת במקרים מסוימים, לכן חשוב לדעת את המבנה של הקובץ והפעולות הנדרשות כד