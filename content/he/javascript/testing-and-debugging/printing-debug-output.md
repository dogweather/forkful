---
title:                "הדפסת פלט לניפוי באגים"
aliases: - /he/javascript/printing-debug-output.md
date:                  2024-01-20T17:53:39.270218-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/printing-debug-output.md"
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
