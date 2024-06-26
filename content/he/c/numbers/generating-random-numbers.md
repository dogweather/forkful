---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:32.337017-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C, \u05E0\u05D9\
  \u05EA\u05DF \u05DC\u05D9\u05D9\u05E6\u05E8 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\
  \u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `rand()`, \u05E9\u05D4\u05D9\
  \u05D0 \u05D7\u05DC\u05E7 \u05DE\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05EA\
  \u05E7\u05DF \u05E9\u05DC C `<stdlib.h>`. \u05DB\u05D1\u05E8\u05D9\u05E8\u05EA \u05DE\
  \u05D7\u05D3\u05DC, `rand()` \u05DE\u05D9\u05D9\u05E6\u05E8\u05EA \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05E4\u05E1\u05D1\u05D3\u05D5-\u2026"
lastmod: '2024-03-13T22:44:40.117503-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D9\u05D9\u05E6\u05E8 \u05DE\
  \u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ `rand()`, \u05E9\u05D4\u05D9\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05D4\u05EA\u05E7\u05DF \u05E9\u05DC C `<stdlib.h>`."
title: "\u05D2\u05D9\u05E8\u05D5\u05D3 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## איך לעשות:
ב-C, ניתן לייצר מספרים אקראיים באמצעות הפונקציה `rand()`, שהיא חלק מספריית התקן של C `<stdlib.h>`. כברירת מחדל, `rand()` מייצרת מספרים פסבדו-אקראיים בטווח מ-0 עד `RAND_MAX` (קבוע המוגדר ב-`<stdlib.h>`). לשם שליטה גבוהה יותר על הטווח, מתכנתים יכולים לתמרן את הפלט של `rand()`.

להלן דוגמא פשוטה של ייצור מספר אקראי בין 0 ל-99:

```c
#include <stdio.h>
#include <stdlib.h> // עבור rand() ו-srand()
#include <time.h>   // עבור time()

int main() {
    // זריעת גנרטור המספרים האקראיים
    srand((unsigned) time(NULL));

    // יצירת מספר אקראי בין 0 ל-99
    int randomNumber = rand() % 100;

    printf("מספר אקראי: %d\n", randomNumber);

    return 0;
}
```

תוצאת הדוגמה עשויה להשתנות בכל פעם שתריץ את התוכנית הזו:

```
מספר אקראי: 42
```
לצורך יצירת מספרים אקראיים בטווח שונה, ניתן להתאים את אופרטור המודולו (`%`) בהתאם. לדוגמה, `rand() % 10` יוצר מספרים בין 0 ל-9.

חשוב לציין, שזריעת גנרטור המספרים הפסבדו-אקראיים (`srand()`) עם הזמן הנוכחי (`time(NULL)`) מבטיחה סדרות שונות של מספרים אקראיים בין הפעלות תוכנית. בלי זריעה (`srand()`), `rand()` היתה מייצרת את אותה הסדרה של מספרים בכל פעם שהתוכנית רצה.

## צלילה עמוקה
הפונקציה `rand()` והתכנונית אשר מזריעה אותה `srand()`, הינן חלק מספריית התקן של C עשורים רבים. הן מבוססות על אלגוריתמים המייצרים סדרות של מספרים שנראות רק כאילו הן אקראיות – ומכאן המונח "פסבדו-אקראיות". האלגוריתם המשמש ב-`rand()` הוא בדרך כלל גנרטור קונגרואלי לינארי (LCG).

למרות ש-`rand()` ו-`srand()` מספיקות עבור רבים מהיישומים, ידועות להן מגבלות, במיוחד בנוגע לאיכות האקראיות והניתנות לחיזוי. עבור יישומים הדורשים אקראיות באיכות גבוהה, כמו פעולות קריפטוגרפיות, יש לשקול חלופות כגון `/dev/random` או `/dev/urandom` (במערכות הפעלה דמויות Unix), או API-ים המוצעים על ידי ספריות קריפטוגרפיות.

עם תחילתו של C11, התקן ISO C כלל כותרת חדשה, `<stdatomic.h>`, המציעה שליטה מדויקת יותר עבור פעולות מקבילות, אך לא באופן ישיר קשורה לאקראיות. לצורך אקראיות אמיתית ב-C, מפתחים לעיתים קרובות פונים לספריות ספציפיות לפלטפורמה או חיצוניות שמציעות אלגוריתמים טובים יותר או מנצלות מקורות אנטרופיה בחומרה.

זכור, כי בעוד `rand()` משמשת כאמצעי פשוט ונגיש לייצור מספרים פסבדו-אקראיים, השימושים שלה ביישומים מודרניים מוגבלים על ידי האיכות והניתנות לחיזוי של הפלט שלה. כאשר פתרונות יותר עמידים נדרשים, בעיקר ליישומים עם מודעות אבטחה, מומלץ לחקור מעבר לספריית התקן.
