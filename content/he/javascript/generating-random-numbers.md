---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:57.251192-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים אקראיים ב-JavaScript זו דרך להפיק ערכים שאין צפויים מראש. תכניתנים זקוקים לזה להגרלות, תהליכים של בדיקת תוכנה, משחקים, וכל גיוון שאתה רק יכול לחשוב עליו.

## איך לעשות:
להפיק מספר אקראי בטווח מ-0 ל-1:
```javascript
console.log(Math.random());
// יוצא משהו כמו 0.8437259416589041
```
לקבלת מספר אקראי בין שני ערכים, `min` ו-`max`:
```javascript
function getRandomBetween(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
console.log(getRandomBetween(1, 10));
// יוצא מספר אקראי בין 1 ל-10
```
להפיק ערך אקראי מתוך מערך:
```javascript
const arr = ['תפוז', 'תפוח', 'בננה', 'ענב'];
console.log(arr[Math.floor(Math.random() * arr.length)]);
// יוצא אחד מהפריטים האלה
```

## עיון מעמיק:
בהיסטוריה, תכניתנים השתמשו במגוון של שיטות ליצירת מספרים אקראיים - מהורגלים מכניים לאלגוריתמים חשמליים. ב-JavaScript, אנחנו משתמשים ב`Math.random()` אבל זה רק נראה אקראי - זו אולי לא הבחירה הטובה ביותר לאבטחה קריטית כמו הצפנה. אלטרנטיבות כוללות אוסף Web Crypto API לדוגמא שמספק גנרטורים של אקראיות קריפטוגרפית. בנוסף, ויתור לגרעין `Math.random()` עצמו יכול להתבצע דרך אלגוריתמים כמו Mersenne Twister, אבל יש לזכור שגם אלה אינם מתאימים לכל מצב, במיוחד לאבטחת מידע.

## גם כדאי לראות:
- [Mozilla MDN Web Docs on Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Mozilla MDN Web Docs on Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
