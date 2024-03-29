---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:11.356184-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D1\u05EA\u05DB\u05E0\u05D5\
  \u05EA, \u05DB\u05D5\u05DC\u05DC\u05EA \u05D0\u05EA \u05D4\u05D1\u05E0\u05D9\u05D9\
  \u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E2\u05DC \u05D9\
  \u05D3\u05D9 \u05D4\u05D8\u05DE\u05E2\u05EA \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\
  \u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\
  \u05D9\u05D8\u05E8\u05DC\u05D9\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D9\
  \u05E6\u05D5\u05E8 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\u05D9\u05D3\u05E2\
  \u05D9\u05D5\u05EA, \u05E9\u05D0\u05D9\u05DC\u05EA\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:40.098118-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05D1\u05EA\u05DB\u05E0\u05D5\
  \u05EA, \u05DB\u05D5\u05DC\u05DC\u05EA \u05D0\u05EA \u05D4\u05D1\u05E0\u05D9\u05D9\
  \u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05E2\u05DC \u05D9\
  \u05D3\u05D9 \u05D4\u05D8\u05DE\u05E2\u05EA \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\
  \u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\
  \u05D9\u05D8\u05E8\u05DC\u05D9\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D9\
  \u05E6\u05D5\u05E8 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\u05D9\u05D3\u05E2\
  \u05D9\u05D5\u05EA, \u05E9\u05D0\u05D9\u05DC\u05EA\u05D5\u05EA\u2026"
title: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

אינטרפולציה של מחרוזות, בתכנות, כוללת את הבנייה של מחרוזות על ידי הטמעת ביטויים בתוך מחרוזות ליטרליות. תכנתים עושים זאת כדי ליצור הודעות מידעיות, שאילתות דינמיות, או לבנות כל מחרוזת עם תוכן משתנה בצורה יעילה ונקייה, לעיתים רבות עבור פלט למשתמש או מטרות רישום.

## איך לעשות:

C, בניגוד לכמה שפות גבוהות יותר, לא תומך באינטרפולציה של מחרוזות באופן ישיר בתחביר שלו. במקום זאת, בניית מחרוזת עם תוכן משתנה מתבצעת בדרך כלל באמצעות הפונקציה `printf` או הגרסאות שלה לפלט, ו-`sprintf` ליצירת מחרוזת. הנה מבט על כיצד לבנות מחרוזות דינמיות ב-C:

```c
#include <stdio.h>

int main() {
    char name[] = "Jane Doe";
    int age = 28;

    // באמצעות printf לפלט
    printf("שלום, שמי %s ואני בן %d.\n", name, age);

    // באמצעות sprintf לבניית מחרוזת
    char info[50];
    sprintf(info, "שם: %s, גיל: %d", name, age);
    printf("%s\n", info);

    return 0;
}
```
פלט לדוגמא:
```
שלום, שמי Jane Doe ואני בן 28.
שם: Jane Doe, גיל: 28
```
הדוגמאות הללו מדגימות את הדרך המסורתית לשלב נתונים משתנים במחרוזות ב-C, ומספקות גמישות בבניית מחרוזות מפורטות.

## טבילה עמוקה

לפני תחילת שפות התכנות המודרניות יותר עם תכונות אינטרפולציה מובנות של מחרוזות, מפתחי C היו צריכים להסתמך על פונקציות כמו `sprintf()`, `snprintf()`, והגרסאות שלהן להרכבת מחרוזות עם תוכן משתנה. גישה זו, למרות שהיא יעילה, מציגה סיכונים פוטנציאליים כמו גלישת באפר אם לא ננקטות זהירות, במיוחד עם `sprintf()`.

בחינת חלופות, שפות כמו Python ו-JavaScript הציגו תכונות אינטרפולציה של מחרוזות יותר אינטואיטיביות, כמו f-strings (מחרוזות ליטרליות מעוצבות) וליטרלי תבנית, בהתאמה. תכונות אלו מאפשרות למפתחים להטמיע ביטויים ישירות בתוך המחרוזות הליטרליות, והופכות את הקוד לקריא ומקוצר יותר.

בהקשר של C, למרות היעדר תכונות אינטרפולציה מובנות של מחרוזות, הגישה שלו מציעה בקרה מדויקת על העיצוב, שניתן לראותה גם כיתרון לאלה הדורשים בקרה מדויקת על העיצוב וגם כמורכבות לחדשים או לאלה המחפשים פתרונות מהירים וקריאים יותר. הצגת `snprintf()` ב-C99 הקלה על חלק מדאגות הבטיחות על ידי אפשרות למפתחים לציין את המספר המרבי של בתים לכתיבה, מה שהפך את עיצוב המחרוזת לבטוח יותר.

למרות ששיטת C עשויה להיראות מסורבלת או מפרכת בהשוואה לשפות מודרניות, הבנת מנגנוני הטיפול במחרוזות שלה מספקת בסיס יציב להבנה של מושגים יותר אבסטרקטיים בפיתוח תוכנה, ומדגישה את חשיבות ניהול הזיכרון ועיצוב הנתונים ברמה נמוכה.
