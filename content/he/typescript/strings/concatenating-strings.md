---
date: 2024-01-20 17:36:24.794599-07:00
description: "\u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05D7\u05EA \u05E9\
  \u05EA\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA (\u05D0\u05D5 \u05D9\u05D5\
  \u05EA\u05E8) \u05D5\u05DC\u05D4\u05D3\u05D1\u05D9\u05E7 \u05D0\u05D5\u05EA\u05DF\
  \ \u05D9\u05D7\u05D3 \u05DC\u05D0\u05D7\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E9\u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05D9\u05E6\u05D5\u05E8 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05E9\u05D5\u05DC\u05D1\
  , \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05E9\u05E4\u05D8\u05D9\u05DD \u05D3\u05D9\
  \u05E0\u05DE\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D4\u05D5\u05E1\u05D9\u05E3\
  \ \u05DE\u05D9\u05D3\u05E2\u2026"
lastmod: '2024-03-13T22:44:38.903994-06:00'
model: gpt-4-1106-preview
summary: "\u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\
  \u05EA \u05D4\u05D5\u05D0 \u05E4\u05E9\u05D5\u05D8 \u05DC\u05E7\u05D7\u05EA \u05E9\
  \u05EA\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA (\u05D0\u05D5 \u05D9\u05D5\
  \u05EA\u05E8) \u05D5\u05DC\u05D4\u05D3\u05D1\u05D9\u05E7 \u05D0\u05D5\u05EA\u05DF\
  \ \u05D9\u05D7\u05D3 \u05DC\u05D0\u05D7\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E9\u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05D9\u05E6\u05D5\u05E8 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05E9\u05D5\u05DC\u05D1\
  , \u05DC\u05D1\u05E0\u05D5\u05EA \u05DE\u05E9\u05E4\u05D8\u05D9\u05DD \u05D3\u05D9\
  \u05E0\u05DE\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D4\u05D5\u05E1\u05D9\u05E3\
  \ \u05DE\u05D9\u05D3\u05E2\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## מה ולמה?
צירופי מחרוזות הוא פשוט לקחת שתי מחרוזות (או יותר) ולהדביק אותן יחד לאחת. תכנתים שעושים זאת כדי ליצור טקסט משולב, לבנות משפטים דינמיים, או להוסיף מידע לפלט.

## איך לעשות:
```typescript
let greeting = 'שלום';
let target = 'עולם';
let message = greeting + ', ' + target + '!'; // צירוף עם אופרטור +
console.log(message); // "שלום, עולם!"

// עם תבנית מיתר (Template Literals)
message = `${greeting}, ${target}!`;
console.log(message); // "שלום, עולם!"

// צירוף מערך של מחרוזות עם מפריד
let colors = ["אדום", "ירוק", "כחול"];
let listOfColors = colors.join(", ");
console.log(listOfColors); // "אדום, ירוק, כחול"
```

## נסוף במבט מעמיק
בעבר, כשהרי קוד לא היו נוחים ומשתמשי עומסים נמוכים, צירוף מחרוזות הוא פעולה שנראתה יקרה. היום, זו פעולה יעילה ושכיחה. ישנן דרכים שונות לצרף מחרוזות ב-TypeScript כמו אופרטור `+` או template literals, שהם יותר קריאים וקלים לניהול. `Array.join()` שימושי כאשר צריך לצרף רשימה של מחרוזות עם מפריד קבוע. במקרה של הביצועים, ההבדלים כיום הם זניחים לרוב השימושים, אבל במידרג רשימות גדולות template literals יכולים להיות יותר יעילים מקטינת שימוש בזיכרון.

## ראה גם
- [MDN String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript Handbook - Template Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#template-strings)
- [JavaScript Performance - Concatenation vs. Template Literals](https://medium.com/@KevinBGreene/javascript-performance-string-concatenation-vs-template-literals-eecbfde1d2d3)
