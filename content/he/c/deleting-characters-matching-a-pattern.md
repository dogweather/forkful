---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "C: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים המתאימים לתבנית היא פעולה שבה תווים מסוימים מוחקים מתוך טקסט לפי תבנית ספציפית. תהליך זה נפוץ בקוד של מתכנתים ומשמש לניקוי טקסט, הסרת תווים לא רצויים או יישומים מתחכמים כמו בדיקת תקינות של כתובת אימייל.

## איך לעשות:
הנה כמה דוגמאות של קוד בשפת C שמדגימות את הפעולה של מחיקת תווים לפי תבנית:

```C
#include <stdio.h>
#include <string.h>

// פונקציה שממחקת את התווים המתאימים לתבנית נתונה מתוך טקסט נתון
void delete_characters(char *text, char *pattern)
{
    int i, j, k;
    int m = strlen(pattern);
    int n = strlen(text);

    // עוברים על כל התווים בטקסט
    for (i = 0; i <= n - m; i++) {
        // בודקים אם התווים תואמים לתבנית
        for (j = 0; j < m; j++) {
            if (text[i+j] != pattern[j])
                break;
        }

        // אם תווים תואמים - מוחקים את התווים שצריך
        if (j == m) {
            for (k = i; k <= n - m; k++)
                text[k] = text[k + m];
                
            // נכתוב מחדש את התו האחרון שנשאר
            text[n - m + 1] = '\0';
        }
    }
}

int main()
{
    char text[] = "Hello world!";
    char pattern[] = "o";

    // קוראים לפונקציה למחיקת התווים
    delete_characters(text, pattern);

    printf("טקסט סופי: %s", text);

    return 0;
}

```
פלט:

טקסט סופי: Hell wrld!

בדוגמה נוספת, קוד זה מסיר תווים שמתאימים לתבנית מהתחלת המחרוזת:

```C
#include <stdio.h>
#include <string.h>

// פונקציה שממחקת את התווים המתאימים לתבנית נתונה מתוך טקסט נתון
void delete_characters(char *text, char *pattern)
{
    int i, j, k;
    int m = strlen(pattern);
    int n = strlen(text);

    // עוברים על כל התווים בטקסט
    for (i = 0; i < n; i++) {
        // בודקים אם התווים תואמים לתבנית
        for (j = 0; j < m; j++) {
            if (text[i+j] != pattern[j])
                break;
        }

        // אם תווים תואמים - מוחקים את התווים שצריך
        if (j == m) {
            for (k = i; k < n - m + 1; k++)
                text[k] = text[k + m];
        }
    }
}

int main()
{
    char text[] = "Hello world!";
    char pattern[] = "He";

    // קוראים לפונקציה למחיקת התווים
    delete_characters(text, pattern);

    printf("טקסט סופי: %s", text);

    return 0;
}

```
פלט:

טקסט סופי: llo world!

## חפירה עמוקה:
מחיקת תווים מתוך טקסט לפי תבנית היא תהליך נפוץ ונרחב בשפת תכנות C. אם ישנם תווים בטקסט שאינם תואמים לתבנית, ניתן להשתמש במספר פתרונות כמו חיפוש והחלפה או פתרון מתקדם יותר באמצעות ביטויים רגולריים. למידע נוסף על ביטויים רגולריים בשפת C ניתן