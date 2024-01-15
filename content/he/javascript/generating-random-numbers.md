---
title:                "יצירת מספרים אקראיים"
html_title:           "Javascript: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# למה

למה ליצור מספרים אקראיים? בכדי להגדיר טווח של מספרים שתחום החיפוש שלך מתאים אליו.

# איך לאפשר מספרים אקראיים ב-Javascript

הנה כמה דוגמאות של קוד ליצירת מספרים אקראיים ב-Javascript, יחד עם הפלט שלהם:

```javascript
// יצירת מספר אקראי בין 0 ל-1
let randomNum = Math.random();
console.log(randomNum); // פלט אפשרי: 0.689173

// יצירת מספר אקראי בין 0 ל-10
let randomNum2 = Math.random() * 10;
console.log(randomNum2); // פלט אפשרי: 6.89173

// יצירת מספר אקראי בין 1 ל-100
let randomNum3 = Math.floor(Math.random() * 100) + 1;
console.log(randomNum3); // פלט אפשרי: 67

// יצירת מספר אקראי בין 0 ל-1000
let randomNum4 = Math.floor(Math.random() * 1000);
console.log(randomNum4); // פלט אפשרי: 874
```

# עומק הנושא: יצירת מספרים אקראיים ב-Javascript

כאשר אנו משתמשים בפונקציות כמו `Math.random()` ב-Javascript, אנו בעצם מייצרים מספרים רנדומליים שנמצאים בתחום מסוים. זה משמעותי כאשר אנו רוצים ליצור מספרים בין טווח מסוים, כגון במשחקים או באתרי האינטרנט. יכולת ליצור מספרים אקראיים מאפשרת יישום כמו טיפול בתורות או תכונות של חפיסות קלפים.

## ראה גם

למידע נוסף על פונקציות מתמטיות ב-Javascript, נתקל בקישורים הבאים:

- [Blog post by fig](https://blog.techbridge.cc/2017/04/22/javascript-r-lean/) (באנגלית)
- [מדריך מכל המקומות](https://javascript.info/number) (באנגלית)
- [Math כמו יותר מ-ספרים](https://www.tutorialsteacher.com/javascript/number-math) (באנגלית)