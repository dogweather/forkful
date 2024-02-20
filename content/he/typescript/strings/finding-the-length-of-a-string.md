---
date: 2024-01-20 17:48:27.179735-07:00
description: "\u05DC\u05DE\u05E6\u05D5\u05D0 \u05D0\u05EA \u05D0\u05D5\u05E8\u05DA\
  \ \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8\
  \ \u05DC\u05E1\u05E4\u05D5\u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D9\u05E9 \u05D1\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\
  \u05D3\u05D5\u05E7 \u05EA\u05E7\u05D9\u05E0\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\
  \u05D7\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05E4\
  \u05E0\u05D9 \u05D4\u05E6\u05D2\u05D4, \u05D5\u05E2\u05D5\u05D3."
lastmod: 2024-02-19 22:04:58.107323
model: gpt-4-1106-preview
summary: "\u05DC\u05DE\u05E6\u05D5\u05D0 \u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05D4\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8 \u05DC\
  \u05E1\u05E4\u05D5\u05E8 \u05DB\u05DE\u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\
  \u05E9 \u05D1\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\
  \u05D5\u05E7 \u05EA\u05E7\u05D9\u05E0\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05D7\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05E4\u05E0\
  \u05D9 \u05D4\u05E6\u05D2\u05D4, \u05D5\u05E2\u05D5\u05D3."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
למצוא את אורך המחרוזת זה פשוט לספור כמה תווים יש בה. תכניתנים עושים את זה כדי לבדוק תקינות קלט, לחתוך מחרוזות לפני הצגה, ועוד.

## איך לעשות:
```TypeScript
// TypeScript מציג את האורך של מחרוזת באמצעות המאפיין `.length`

let greeting: string = 'שלום עולם';
console.log(greeting.length);  // יודפס: 10
```
שימו לב שב-JavaScript ו-TypeScript, כל תו שווה לתו אחד, גם אם הוא תו מיוחד כמו אמוג'י.

## עיון עמוק:
בעבר, כשמערכות הפעלה ושפות תכנות עוד היו בחיתוליהן, לא תמיד ניתן היה לדעת בקלות כמה תווים יש במחרוזת. במקרים רבים היה צורך ללכת על כל המחרוזת תו-תו עד שמגיעים לתו סיום.

כיום, ב-TypeScript, מאפיין ה`.length` מחזיר את אורך המחרוזת תוך חישוב מיידי, קל ופשוט. אך חשוב להבין שיכולות להיות הבדלים בין אורך המחרוזת למספר היחידות קוד ב-UTF-16 שמתייחסים לתווים המורכבים משני "surrogate pairs", כמו אמוג'ים מסוימים או תווי שפה מתקדמת.

אלטרנטיבות ל-TypeScript כוללות שימוש בביטויים רגולריים או פונקציות ספירה מותאמות אישית, אך ברוב המקרים, `.length` יעשה את העבודה טוב ומהר.

## ראה גם:
- [String.length MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length) - מידע על מאפיין ה`.length` במחרוזת.
- [Unicode characters and JavaScript strings](https://mathiasbynens.be/notes/javascript-unicode) - פוסט שמסביר על היחס בין תווים יוניקוד למחרוזות ב-JavaScript.
- [TypeScript Handbook - Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html) - מדריך של TypeScript על סוגי נתונים בסיסיים.
