---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:43.905511-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC, \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05E6\
  \u05D5\u05E8 \u05D3\u05E4\u05D5\u05E1 regex \u05E4\u05E9\u05D5\u05D8 \u05D5\u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05DC\u05DE\u05E6\u05D9\u05D0\u05EA\
  \ \u05D4\u05EA\u05D0\u05DE\u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  . \u05DB\u05D0\u05DF, \u05E0\u05DE\u05E6\u05D0 \u05D0\u05EA \u05D4\u05DE\u05D9\u05DC\
  \u05D4 \"code\"."
lastmod: '2024-03-13T22:44:39.958519-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC, \u05D0\u05E4\u05E9\
  \u05E8 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D3\u05E4\u05D5\u05E1 regex \u05E4\u05E9\
  \u05D5\u05D8 \u05D5\u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05DC\u05DE\
  \u05E6\u05D9\u05D0\u05EA \u05D4\u05EA\u05D0\u05DE\u05D5\u05EA \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות:


### התאמה בסיסית
כדי להתחיל, אפשר ליצור דפוס regex פשוט ולהשתמש בו למציאת התאמות במחרוזת. כאן, נמצא את המילה "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### שימוש ב-`String.prototype.match()`
לאחזור על מערך של התאמות:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### חיפוש כללי
כדי למצוא את כל ההתאמות, יש להשתמש בדגל `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### התאמה ללא תלות ברישיות
הדגל `i` מתעלם מהרישיות:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### החלפת טקסט
השתמשו ב-`String.prototype.replace()` להחלפת חלקים במחרוזת:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### שימוש בקבוצות
קבוצות יכולות לתפוס חלקים מהדפוס:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### ספריות צד שלישי
למרות שיכולות ה-regex המובנות ב-JavaScript חזקות, חלק מהמשימות יכולות להפשט עם ספריות כמו `XRegExp`. היא מציעה תחביר ודגלים נוספים, מה שהופך דפוסים מורכבים לקריאים יותר:

```javascript
// דוגמת ספריית XRegExp
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

הקטע הזה מדגים שימוש ב-`XRegExp` להתאמת כל המילים ב-Unicode במחרוזת, מה שמציג את יכולת הספרייה לטפל בסטים של תווים מורחבים מעבר ליכולות המובנות של JavaScript.
