---
date: 2024-01-20 17:39:26.054863-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DC\u05D4\u05D7\u05DC\u05D9\u05E3 \u05D0\u05EA \u05DB\
  \u05DC \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D2\u05E8\u05E1\u05EA\u05DD \u05D4\u05E7\u05D8\u05E0\u05D4. \u05EA\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05E2\
  \u05E7\u05D1\u05D9\u05D5\u05EA, \u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05D1\u05E6\
  \u05D5\u05E8\u05D4 \u05E8\u05D2\u05D9\u05E9\u05D4 \u05DC\u05E8\u05D9\u05E9\u05D9\
  \u05D5\u05EA \u05D0\u05D5 \u05DC\u05D8\u05E4\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.895930-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8 \u05DC\u05D4\u05D7\u05DC\u05D9\u05E3 \u05D0\u05EA \u05DB\u05DC\
  \ \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D2\u05E8\u05E1\u05EA\u05DD \u05D4\u05E7\u05D8\u05E0\u05D4."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## איך לעשות:
```typescript
let greeting: string = 'שלום עולם!';
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting); // 'שלום עולם!' stays the same because it's already in lower case.
```

פלט לדוגמא:
```
שלום עולם!
```

כדי להשוות בין שתי מחרוזות בלי לדאוג לרישיות:
```typescript
let userInput: string = 'Email@Example.com';
let storedEmail: string = 'email@example.com';

if (userInput.toLowerCase() === storedEmail.toLowerCase()) {
  console.log('מיילים תואמים!');
} else {
  console.log('מיילים לא תואמים.');
}
```

פלט לדוגמא:
```
מיילים תואמים!
```

## צלילה עמוקה
בימים הראשונים, תכנות היה עם מעט מאוד שפות, וכל עבודה עם מחרוזות הייתה ידנית. ככל שתכנות התפתח, שפות כוללות טיפול במחרוזות בנותינות יותר כמו JavaScript ו-TypeScript. 

אלטרנטיבות? ניתן גם להשתמש בפונקציות סטנדרטיות כמו `String.prototype.toUpperCase` להפך, או ב-RegExp להחלפות מורכבות יותר.

לגבי פרטי יישום, `toLowerCase` מנוהלת על ידי כללי ה-UniCode והספריית הסטנדרט של JavaScript כדי להבטיח שתווים מקבצים לא לטיניים יופחתו כראוי ללא בעיות.

## ראה גם
- מסמך MDN Web Docs על `toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Unicode Case Mapping: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- תיעוד על מחרוזות ב-TypeScript: [TypeScript Handbook Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
