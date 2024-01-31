---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:54:19.536049-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)

קריאת קובץ טקסט ב-C מאפשרת לתוכנה לטעון ולעבד נתונים מקובץ. תכניתנים עושים זאת לקרוא קלט, לעדכן מאגר נתונים, או לבצע אנליזות.

## How to: (איך לעשות:)

קטע הקוד הבא מראה איך לקרוא מקובץ טקסט ב-C:

```C
#include <stdio.h>

int main() {
    FILE *fileptr;
    char filename[] = "example.txt";
    char ch;

    fileptr = fopen(filename, "r"); // פתח את הקובץ לקריאה

    if (fileptr == NULL) {
        perror("Error opening file");
        return -1;
    }

    while ((ch = fgetc(fileptr)) != EOF) { // קרא תווים עד סוף הקובץ
        putchar(ch); // הדפס תו לקונסול
    }

    fclose(fileptr); // סגור את הקובץ
    return 0;
}
```

הפלט הייה תכני הטקסט שב"example.txt" מודפס לקונסול.

## Deep Dive (צלילה עמוקה)

קריאת קבצים היא חלק יסודי בתכנות עוד מהימים שבהם מחשבים עבדו על פקודות פאנץ'. ישנם אלטרנטיבות ל-`fgetc`, כמו `fgets` ו`fread`, שמאפשרות קריאה של שורת טקסט או בלוק של נתונים בפעם. הבחירה תלויה בצורת נתונים שאתה מעוניין לעבד. פתיחת קובץ במצב 'r' היא לקריאה בלבד, שומרת על הנתונים משינוי בלתי מתוכנן. העבודה עם פונקציות קריאת קובץ דורשת ניהול זהיר של זכרונות והקפדה על סגירת הקובץ לאחר השימוש בו כדי למנוע נזילות זיכרון.

## See Also (ראה גם)

- תיעוד הפונקציות `fopen`, `fgetc`, `fgets`, ו-`fclose` במאגר C הסטנדרטי: http://www.cplusplus.com/reference/cstdio/
- מדריך לעבודה עם קבצי טקסט ב-C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- דוגמאות לקריאת קבצים ב-C: https://www.geeksforgeeks.org/basics-file-handling-c/
