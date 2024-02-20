---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:45.533054-07:00
description: "\u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA\
  \ \u05DB\u05D5\u05DC\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05E7\
  \u05DC\u05D8 \u05DB\u05DA \u05E9\u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05DF \u05D9\u05D4\u05D9\u05D4 \u05D1\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\
  \u05D4 \u05D1\u05E2\u05D5\u05D3 \u05D4\u05E9\u05D0\u05E8 \u05D9\u05D9\u05E9\u05D0\
  \u05E8\u05D5 \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\
  \u05EA, \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\u05E4\u05D5\u05E5 \u05DC\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E9\u05DC \u05E9\u05DE\u05D5\u05EA \u05D0\u05D5 \u05DB\u05D5\
  \u05EA\u05E8\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:57.794340
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05E4\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05D5\u05EA \u05DB\
  \u05D5\u05DC\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05D4\u05E7\u05DC\
  \u05D8 \u05DB\u05DA \u05E9\u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\u05DF\
  \ \u05D9\u05D4\u05D9\u05D4 \u05D1\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  \ \u05D1\u05E2\u05D5\u05D3 \u05D4\u05E9\u05D0\u05E8 \u05D9\u05D9\u05E9\u05D0\u05E8\
  \u05D5 \u05D1\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  , \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E0\u05E4\u05D5\u05E5 \u05DC\u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05E9\u05DC \u05E9\u05DE\u05D5\u05EA \u05D0\u05D5 \u05DB\u05D5\u05EA\
  \u05E8\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

להפוך מחרוזת לאותיות ראשיות כולל שינוי של הקלט כך שהתו הראשון יהיה באות גדולה בעוד השאר יישארו באותיות קטנות, שימוש נפוץ לפורמט של שמות או כותרות. תכנתים עושים זאת כדי להבטיח עקביות של נתונים ולשפר את נוחות הקריאה בתוך ממשקי משתמש או מסמכים.

## איך לעשות:

Google Apps Script, המבוסס על JavaScript, מאפשר מספר שיטות להפוך מחרוזת לאותיות ראשיות, אף על פי שאין בו פונקציה מובנית. הנה כמה דוגמאות תמציתיות:

**שיטה 1: שימוש ב-charAt() ו-slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// שימוש לדוגמה
let result = capitalizeString('hello, world');
console.log(result);  // פלט: Hello, world
```

**שיטה 2: שימוש ב-Regex**

למי שמעדיף פתרון מבוסס regex לטיפול במקרי קצה בצורה יותר אלגנטית:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// שימוש לדוגמה
let result = capitalizeStringRegex('hello, world');
console.log(result);  // פלט: Hello, world
```

שתי השיטות מבטיחות שהתו הראשון של המחרוזת יהיה באות ראשית, והשאר באותיות קטנות, מתאים למגוון יישומים כולל אך לא מוגבל לתפעול גיליונות Google או עריכת מסמכים באמצעות Apps Script.

## צלילה עמוקה

להפוך מחרוזות לאותיות ראשיות ב-Google Apps Script הוא תהליך ישיר, באמצעות שימוש ביכולות העוצמתיות של ניהול מחרוזות ב-JavaScript. בעבר, שפות כמו Python הציעו שיטות מובנות כמו `.capitalize()` לביצוע זה, מה שנותן צעד נוסף לתכנתים ב-JavaScript וב-Apps Script. עם זאת, העדר פונקציה מובנית ב-JavaScript/Google Apps Script מעודד גמישות והבנה עמוקה יותר של טכניקות ניהול מחרוזות.

לסיטואציות מורכבות יותר, כמו להפוך כל מילה במחרוזת לאות ראשית (Title Case), תכנתים עשויים לשלב שיטות regex עם הפונקציות `split()` ו-`map()` כדי לעבד כל מילה בנפרד. למרות ש-Google Apps Script לא מספקת שיטה ישירה להפוך מחרוזות לאותיות ראשיות, השימוש בשיטות ניהול מחרוזות של JavaScript קיימות מציע גמישות רבה, מאפשרת למפתחים לטפל במחרוזות ביעילות לפי הצורך הספציפי שלהם.

במקרים בהם הביצועים והיעילות הם בעדיפות ראשונה, כדאי להדגיש כי ניהול מחרוזות ישיר עשוי להיות יותר יעיל מאשר regex, במיוחד עבור מחרוזות ארוכות יותר או פעולות בלולאות גדולות. עם זאת, לרוב היישומים המעשיים ב-Google Apps Script, שתי הגישות מספקות פתרונות אמינים.
