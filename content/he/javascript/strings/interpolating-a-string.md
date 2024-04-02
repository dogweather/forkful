---
date: 2024-01-20 17:51:14.654867-07:00
description: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D1-JavaScript \u05D4\u05D5\u05D0 \u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D5\
  \ \u05DE\u05D6\u05E8\u05D9\u05E7\u05D9\u05DD \u05E2\u05E8\u05DB\u05D9\u05DD \u05D3\
  \u05D9\u05E0\u05DE\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D8\u05E7\u05E1\u05D8. \u05D6\u05D4 \u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D9\u05E2\u05D9\u05DC\u05D4 \u05D5\u05E7\
  \u05E8\u05D9\u05D0\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05DC \u05E7\u05D5\u05D3\
  \ \u05E2\u05DC \u05D9\u05D3\u05D9 \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E2\u05DD \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E6\u05D5\u05E8\
  \u05D4\u2026"
lastmod: '2024-03-13T22:44:39.952208-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D9\u05DC\u05D5\u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\
  -JavaScript \u05D4\u05D5\u05D0 \u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D5 \u05DE\
  \u05D6\u05E8\u05D9\u05E7\u05D9\u05DD \u05E2\u05E8\u05DB\u05D9\u05DD \u05D3\u05D9\
  \u05E0\u05DE\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D8\u05E7\u05E1\u05D8. \u05D6\u05D4 \u05DE\u05D0\u05E4\u05E9\u05E8\
  \ \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D9\u05E2\u05D9\u05DC\u05D4 \u05D5\u05E7\u05E8\
  \u05D9\u05D0\u05D4 \u05D9\u05D5\u05EA\u05E8 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05E2\
  \u05DC \u05D9\u05D3\u05D9 \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05E2\u05DD \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E6\u05D5\u05E8\u05D4\
  \u2026"
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## מה ולמה?
מילוי מחרוזת ב-JavaScript הוא כאשר אנו מזריקים ערכים דינמיים לתוך מחרוזת טקסט. זה מאפשר כתיבה יעילה וקריאה יותר של קוד על ידי שילוב נתונים עם טקסט בצורה נקייה ואלגנטית.

## איך לעשות:
ב-JavaScript, אנחנו משתמשים בתבניות מחרוזת (string templates) כדי לבצע מילוי. הנה דוגמה:

```Javascript
let firstName = 'ישראל';
let lastName = 'ישראלי';
let greeting = `שלום, ${firstName} ${lastName}!`;

console.log(greeting); // פלט: שלום, ישראל ישראלי!
```
וכך זה נראה עם פונקציות:

```Javascript
function getWelcomeMessage(name) {
  return `ברוך הבא, ${name}!`;
}

console.log(getWelcomeMessage('דני')); // פלט: ברוך הבא, דני!
```

## צלילה לעומק:
מילוי מחרוזות שייך לעולם התיכנות מדורות ראשונים. ב-JavaScript, עד ES5, למתכנתים הוצעו אפשרויות כמו שילוב עם אופרטור החיבור (`+`). ES6 הביא את הסינטקס עם סימני גרש הנקראים Template Literals, ושינה את המשחק. זה לא רק פשט את הקוד, אלא גם הפחית את הטעויות והבלבול.

דוגמאות לחלופות: בעבר, פונקציות כמו `sprintf()` או פתרונות של תוספות שלדים (frameworks) עשו את העבודה. כיום, פונקציות כמו `replace()` או ספריות כמו Lodash יכולות להציע גמישות רבה יותר. 

פרטים טכניים כוללים הבנת התחביר כולל השימוש ב`${}` והיכולת לשלב ביטויים ישירות לתוך המחרוזת, לחשב אותם במהלך הריצה, במיוחד עם ביטויים מסובכים או קריאות לפונקציות.

## ראו גם:
- [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) - מדריך רשמי בנושא תבניות מחרוזת.
- [MDN String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) - כל המידע על אובייקט String ב-JavaScript.
