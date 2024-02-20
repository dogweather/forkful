---
date: 2024-01-20 17:53:39.270218-07:00
description: "\u05D3\u05D9\u05D1\u05D0\u05D2 (Debug) \u05D4\u05D5\u05D0 \u05D4\u05EA\
  \u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\
  \u05D6\u05D4\u05D9\u05DD \u05D5\u05DE\u05EA\u05E7\u05E0\u05D9\u05DD \u05EA\u05E7\
  \u05DC\u05D5\u05EA \u05D1\u05E7\u05D5\u05D3. \u05D0\u05D7\u05EA \u05D4\u05D3\u05E8\
  \u05DB\u05D9\u05DD \u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D5\u05D4\u05DE\
  \u05D4\u05D9\u05E8\u05D5\u05EA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D3\u05D9\u05D1\
  \u05D0\u05D2 \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05D3\u05E4\
  \u05E1\u05EA \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC\
  . \u05DB\u05DA \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD\
  \ \u05DC\u05E8\u05D0\u05D5\u05EA\u2026"
lastmod: 2024-02-19 22:04:59.250938
model: gpt-4-1106-preview
summary: "\u05D3\u05D9\u05D1\u05D0\u05D2 (Debug) \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D6\
  \u05D4\u05D9\u05DD \u05D5\u05DE\u05EA\u05E7\u05E0\u05D9\u05DD \u05EA\u05E7\u05DC\
  \u05D5\u05EA \u05D1\u05E7\u05D5\u05D3. \u05D0\u05D7\u05EA \u05D4\u05D3\u05E8\u05DB\
  \u05D9\u05DD \u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D5\u05D4\u05DE\u05D4\
  \u05D9\u05E8\u05D5\u05EA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D3\u05D9\u05D1\u05D0\
  \u05D2 \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05D3\u05E4\u05E1\
  \u05EA \u05DE\u05D9\u05D3\u05E2 \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC. \u05DB\
  \u05DA \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\
  \u05E8\u05D0\u05D5\u05EA\u2026"
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
דיבאג (Debug) הוא התהליך שבו אנחנו מזהים ומתקנים תקלות בקוד. אחת הדרכים הפשוטות והמהירות לעשות דיבאג היא על ידי הדפסת מידע לקונסול. כך אנחנו יכולים לראות מה קורה "מתחת לכובע" בזמן אמת ולאתר בעיות.

## איך לעשות:
```Javascript
// הדפסת מידע בסיסי
console.log('שלום, עולם!');

// הדפסה עם מספר ואובייקטים
console.log(3);
console.log({שם: 'יוסי', גיל: 30});

// עיצוב המודעה על ידי סגנונות CSS
console.log('%cהדפסה בסגנון', 'color: blue; font-weight: bold');

// הדפסת אזהרה ושגיאה
console.warn('זו אזהרה!');
console.error('זו שגיאה!');

// הדפסת JSON בצורה נקייה
console.log(JSON.stringify({שם: 'יוסי', גיל: 30}, null, 2));
```

תוצאות הדוגמא:
```
שלום, עולם!
3
{שם: "יוסי", גיל: 30}
הדפסה בסגנון
זו אזהרה!
זו שגיאה!
{
  "שם": "יוסי",
  "גיל": 30
}
```

## שיטות מתקדמות:
ההדפסה לקונסול היא כלי שהיה קיים כמעט משחר התיכנות. בעידן הדפדפנים, היא קיבלה יותר יכולות, כולל סינון הודעות, עיצוב עם CSS וקבוצות הודעות.
קיימות גם אלטרנטיבות מתקדמות, כמו כלי דיבאג שמובנים ב-IDEs או תוספים לדפדפן כמו Firebug שהיה פופולרי בעבר. כמו כן, קיימת הכרח בידע בכל הנוגע להבחנת זמן מועיל להשקעה ב-logging נכון לעומת שימוש בכלים מתקדמים יותר כמו profilers, נטוורק monitors או אחרים.

## ראו גם:
- [console - Web APIs | MDN](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp)
- [Using Your Browser to Diagnose JavaScript Errors](https://codex.wordpress.org/Using_Your_Browser_to_Diagnose_JavaScript_Errors)
