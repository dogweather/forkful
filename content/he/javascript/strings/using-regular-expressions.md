---
title:                "שימוש בביטויים רגולריים"
date:                  2024-02-03T19:17:43.905511-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים (regex) ב-JavaScript הם דפוסים המשמשים להתאמת שילובי תווים במחרוזות. מתכנתים משתמשים בהם לחיפוש, חילוץ ועיבוד טקסט, מה שמאפשר פעולות עיבוד מחרוזות חזקות עם קוד תמציתי.

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
