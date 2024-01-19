---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

סילוק מספרים אקראיים הוא מנגנון שבו מחולל מספרים באופן רנדומלי. תכנתים משתמשים בו כדי ליצור נתונים לא צפויים ובלתי נאמתים, דבר שמממש את האינטראקציה והגיוון ביישומים.

## כיצד: 

ב-JavaScript, אפשר להשתמש במתודה Math.random() כדי ליצור מספר אקראי בין 0 (למעלה) ו-1 (נמוך):

```JavaScript
console.log(Math.random());
```

בפעם הראשונה שתריץ את הקוד, אתה עשוי לראות משהו כמו:

```JavaScript
0.12345678901234567
```

אם אתה רוצה ליצור מספר בין שני מספרים, אתה יכול להוסיף קצת מתמטיקה פשוטה:

```JavaScript
var min = 10;
var max = 100;
console.log(Math.floor(Math.random() * (max - min + 1) + min));
```

כל פעם שתריץ את הקוד, תקבל מספר אקראי בין 10 ל-100.

## צלילה עמוקה:

גירוי מספרים אקראיים באינטרנט זמין מאז שנות ה-50, והוא משמש את מגוון רחב של תחומים, לרבות משחקים, הצפנה, אמנות והפרת נתונים. מתודה Math.random() של JavaScript מבוססת על אלגוריתם שנקרא "linear congruential generator", שהוא אחד האלגוריתמים הקלאסיים ליצירת מספרים אקראיים.

אפשרויות חלופות ב-JavaScript כוללות crypto.getRandomValues() עבור תרחישים שלא נתמכים ב-Math.random(), וביבליות חיצוניות כמו uuid עבור מחולל מזהים ייחודיים.

## ראה גם:

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random
https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
https://www.w3schools.com/js/js_math.asp