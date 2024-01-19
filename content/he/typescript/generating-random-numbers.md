---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה אומר ולמה?

אפשרות ליצור מספרים אקראיים היא כלי חיוני בעידן הדיגיטלי. זה שימושי בעיקר לבניית פונקציונליות כמו משחקים, אבטחת נתונים, אלגוריתמים של מחשב, ועוד.

## איך לעשות:

בקוד TypeScript, ניתן להשתמש בפונקציה `Math.random()` כדי להפיק מספר אקראי בין 0 כולל ו-1 לא כולל.

```TypeScript
let randomNum = Math.random();
console.log(randomNum);
```

דוגמה לפלט:

```
0.3453456534687663
```

כדי להפיק מספר אקראי בין שני מספרים מסוימים, אנחנו מכפילים את המספר האקראי במרחק בין המספרי המקסימלי למינימלי ומוסיפים את המספר המינימלי.

```TypeScript
let randomInRange = (min: number, max: number) => {
  return Math.random() * (max - min) + min;
}
console.log(randomInRange(10, 20));
```

דוגמה לפלט:

```
15.34565343
```

## צלילה עמוקה:

בגילות המערה, אנשים לא הכירו מספרים רנדומליים. אך במאה ה-20 נהפך זה לנושא עיון מרכזי במתמטיקה ומדעי המחשב. אלגוריתם ליצירת מספרים רנדומאליים חייב להיות "נטול קיריאה" - הפלט לא ניתן לחזות מהפלטים הקודמים.

חלופותיה ל-`Math.random()` כוללות את השימוש בספריות חיצוניות כמו `crypto-js` או `uuid` שמספקות פונקציות אקראיות עם רמת אבטחה גבוהה יותר.

מימוש מספר אקראי ב-`Math.random()` מתבצע באמצעות אלגוריתמים מתמטיים מורכבים שמבצעים פעולות על מספרים ראשוניים ולבסוף מחזירים תוצאה בין 0 ו-1.

## ראה גם:

* [MDN Web Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
* [npm: crypto-js](https://www.npmjs.com/package/crypto-js)
* [npm: uuid](https://www.npmjs.com/package/uuid)