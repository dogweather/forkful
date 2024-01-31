---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות (Tests) בקוד מאפשרת לגלות מוקדם באגים ולשמור על איכות התוכנה. מתכנתים כותבים בדיקות כדי לאמת שהקוד עובד כצפוי לפני שיוצאים לאוויר.

## איך לעשות:
דוגמא לקוד עם Jest, ספריית בדיקות פופולארית ב-JavaScript:

```javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;

// sum.test.js
const sum = require('./sum');

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});

// הרצה בקונסולה:
// $ jest
// PASS  ./sum.test.js
// ✓ adds 1 + 2 to equal 3 (5ms)
```

זה דוגמה פשוטה של פונקציה שמחזירה סכום של שני מספרים ובדיקה שמאמתת שהתוצאה נכונה.

## טבילה עמוקה
כתיבת בדיקות החלה עוד בשנות ה-50 ודרך השנים פותחו שיטות וכלים רבים. Jest, Mocha ו-Jasmine הם עכשיו האלטרנטיבות הפופולאריות בעולם ה-JavaScript. כל ספרייה מציעה חומרה ואפשרויות שונות, אבל הקונספט דומה: הגדרת בדיקות (tests) שמתבצעות על הקוד שלך להבטחת איכות.

## ראו גם
- [Jest](https://jestjs.io/) - התיעוד הרשמי של Jest, הכלי לכתיבת בדיקות.
- [Mocha](https://mochajs.org/) - תיעוד Mocha, עוד פריימוורק לבדיקות.
- [Jasmine](https://jasmine.github.io/) - עמוד הבית של Jasmine, ספריית בדיקות התנהגותית.
- [JavaScript Testing Best Practices](https://github.com/goldbergyoni/javascript-testing-best-practices) - אוסף של שיטות מומלצות לבדיקות JavaScript.
