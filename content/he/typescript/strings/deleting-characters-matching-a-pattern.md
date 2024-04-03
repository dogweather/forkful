---
date: 2024-01-20 17:44:10.850158-07:00
description: "\u05D1 TypeScript, \u05DC\u05DE\u05D7\u05D5\u05E7 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D4\u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\
  \u05EA \u05D6\u05D4 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E7\u05D8\u05E2\
  \u05D9 \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DC\u05D4\u05E1\u05D9\u05E8 \u05D0\u05EA\
  \ \u05DE\u05D4 \u05E9\u05D0\u05D9\u05E0\u05E0\u05D5 \u05E8\u05E6\u05D5\u05D9. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DC\u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05E9\u05E4\u05E8\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\
  \u05D4\u05EA\u05D0\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: '2024-03-13T22:44:38.891315-06:00'
model: gpt-4-1106-preview
summary: "\u05D1 TypeScript, \u05DC\u05DE\u05D7\u05D5\u05E7 \u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05D4\u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\u05EA\
  \ \u05D6\u05D4 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E7\u05D8\u05E2\u05D9\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DC\u05D4\u05E1\u05D9\u05E8 \u05D0\u05EA \u05DE\
  \u05D4 \u05E9\u05D0\u05D9\u05E0\u05E0\u05D5 \u05E8\u05E6\u05D5\u05D9."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
כדי למחוק תווים שתואמים תבנית ב-TypeScript, אתה יכול להשתמש בביטויים רגולריים עם המתודה `replace`. דוגמה:

```typescript
const cleanString = (str: string, pattern: RegExp): string => {
  return str.replace(pattern, '');
};

const exampleString = 'Hey! How are you doing today?';
const cleanedString = cleanString(exampleString, /[?!]/g); // הסרת סימני שאלה וקריאה

console.log(cleanedString); // הדפסת התוצאה
```

פלט דוגמה:

```
Hey How are you doing today
```

## נפילה לעומק:
ביטויים רגולריים (Regex) הם כלי חזק בכל שפת תכנות, וכבר קיימים מאז שנות ה-60. ב-TypeScript, שהיא הרחבה קטנועית של JavaScript, השימוש ב-Regex דומה מאוד. מלבד `replace`, יש גם שיטות אחרות כמו `match` ו-`search` שמאפשרות עבודה עם תבניות. ב-Symbols חדשניים ב-RegExp יכולים לשפר את הביצועים בהתאם לפעולות שביצעת – למשל, שימוש בסימן `g` לחיפוש גלובלי.

האם יש חלופות ל-RegExp? כן, דרך אחרת היא להשתמש במתודות מחרוזת כמו `split` ו-`join`. זה יכול להיות אפקטיבי אם אתה צריך להסיר תווים פשוטים, אבל במקרים מורכבים, Regex הוא כלי עוף.

## ראה גם:
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [RegExp שימושים וטיפים](https://regexr.com/)
