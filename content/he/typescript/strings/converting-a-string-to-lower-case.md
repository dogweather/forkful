---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- /he/typescript/converting-a-string-to-lower-case/
date:                  2024-01-20T17:39:26.054863-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות זה פשוט להחליף את כל התווים במחרוזת לגרסתם הקטנה. תכניתנים עושים את זה כדי להבטיח עקביות, להשוות בצורה רגישה לרישיות או לטפל בקלט משתמש.

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
