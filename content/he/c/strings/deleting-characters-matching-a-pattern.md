---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:02.854114-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05EA \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D1\u05E9\u05E4\u05EA C \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D4\
  \u05E1\u05E8\u05EA \u05DB\u05DC \u05D4\u05DE\u05D5\u05E4\u05E2\u05D9\u05DD \u05E9\
  \u05DC \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD\
  \ \u05D4\u05DE\u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05E7\u05E8\u05D9\u05D8\
  \u05E8\u05D9\u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D2\u05D3\u05E8\u05D9\u05DD\
  \ \u05DE\u05E8\u05D0\u05E9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\
  \u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D6\u05D5\u2026"
lastmod: '2024-03-13T22:44:40.094045-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\
  \u05D5\u05D9\u05DE\u05EA \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\
  \u05E9\u05E4\u05EA C \u05DE\u05D3\u05D5\u05D1\u05E8\u05EA \u05E2\u05DC \u05D4\u05E1\
  \u05E8\u05EA \u05DB\u05DC \u05D4\u05DE\u05D5\u05E4\u05E2\u05D9\u05DD \u05E9\u05DC\
  \ \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05D4\
  \u05DE\u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05E7\u05E8\u05D9\u05D8\u05E8\u05D9\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05D5\u05D2\u05D3\u05E8\u05D9\u05DD \u05DE\u05E8\
  \u05D0\u05E9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\
  \u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D6\u05D5\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05DE\
  \u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

מחיקת תווים התואמים לתבנית מסוימת ממחרוזות בשפת C מדוברת על הסרת כל המופעים של תווים מסוימים המתאימים לקריטריונים מוגדרים מראש. מתכנתים מבצעים משימה זו כדי לסנן קלטים, להכין נתונים לעיבוד, או פשוט לנקות מחרוזות לצורך הצגה או מניפולציה נוספת, ובכך להבטיח שהנתונים שבהם מתמודדים הם בדיוק כפי שנדרש להקשר או לאלגוריתם נתון.

## איך ל:

C אינה מגיעה עם פונקציה מובנית למחיקת תווים ממחרוזת בהתבסס על תבנית, בניגוד לחלק מהשפות ברמה גבוהה יותר. עם זאת, ניתן לבצע משימה זו בקלות על ידי איטרציה ידנית על המחרוזת ובניית מחרוזת חדשה שאינה כוללת את התווים הלא רצויים. לדוגמה, בואו נניח שאתם רוצים להסיר את כל הספרות ממחרוזת. תוכלו לעשות זאת כך:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C Programming 101: The Basics!";
    remove_digits(str);
    printf("Result: %s\n", str);
    return 0;
}
```

דוגמה לפלט:
```
Result: C Programming : The Basics!
```

דוגמה זו משתמשת בפונקציה `isdigit` מ`ctype.h` כדי לזהות ספרות, תוך הזזת תווים שאינם ספרות לתחילת המחרוזת וסיום המחרוזת לאחר הערכת כל התווים.

## צלילה עמוקה

הפתרון המוצג עושה שימוש בגישת שני מצביעים באותו מערך כדי לסנן באופן יעיל תווים לא רצויים, טכניקה המסמלת את פילוסופיית ניהול הזיכרון הידני האופיינית ל-C. שיטה זו היא יעילה כיוון שהיא פועלת במקום, מה שמקטין את הצורך בהקצאת זיכרון נוסף ובכך מפחית את העומס.

היעדר פונקציות מובנות לניהול מחרוזות ברמה גבוהה ב-C לעיתים קרובות דורש ממתכנתים לפתח הבנה עמוקה של טיפול במחרוזות ברמת הזיכרון, מה שמוביל לגישות חדשניות כמו המוצגת למעלה. למרות שיתרון זה של שליטה גבוהה יותר ויעילות, הוא מלווה בסיכון גבוה יותר לשגיאות, כמו גלישות מעבר לגבולות המערך וטעויות שלאחד-מינוס.

בהקשרים מודרניים לפיתוח, במיוחד אלו שמדגישים בטיחות ואבטחה, שפות שמסתירות פעולות ברמה נמוכה כגון זו עשויות להיות מועדפות למשימות של טיפול במחרוזות. עם זאת, הבנה ושימוש בטכניקות של C נותרים בלתי יקרים עבור תרחישים הדורשים אופטימיזציה עדינה של ביצועים או לעבודה בסביבות שבהן המינימליזם והמהירות של C הם הכרחיים.
