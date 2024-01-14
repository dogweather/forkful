---
title:    "Javascript: יצירת מספרים אקראיים"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## למה
אנשים מתעניינים ביצירת מספרים אקראיים משום שהם יכולים לשמש כאמצעי ליצירת ארועים רנדומליים ולשיפור המשחקים והיישומים שהם פועלים בהם.

## איך לעשות זאת
ניתן ליצור מספרים אקראיים באמצעות שני שיטות: Math.random () ו- Math.floor (). השיטה הראשונה מפיצה מספרים עשרוניים בין 0 ל- 1, והשנייה מציגה מספרים שלמים בין מספר התחלה ומספר סוף. ניתן להשתמש בפונקציות אלה יחד כדי ליצור מספרים אקראיים בטווח מסוים, ולהציג את התוצאות באמצעות פקודות כמו console.log ().

```Javascript
// יצירת מספר רנדומלי בין 1 ל-10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);

// הדפסת מספר רנדומלי בין 100 ל- 1000
let randomNumber = Math.floor(Math.random() * 901) + 100;
console.log(randomNumber);
```

## מעמקים
יצירת מספרים אקראיים היא תהליך מתמטי מורכב המשתמש באלגוריתמים ובנתונים מסוימים כדי ליצור את המספרים הנדרשים. קיימים גם אלגוריתמים מתקדמים שמשתמשים במספרי טבע ובזמן ליצירת מספרים בלתי חוזרים.

## ראו גם
- [ליצירת מספרים אקראיים ב-Javascript](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [השפעת מספרים אקראיים על משחקים ויישומים](https://www.gamasutra.com/view/feature/134394/random_number_generation.php)
- [אלגוריתמי גנרטורים לפי תוך עלייה בסיכון](https://medium.com/the-liberal-game/introduction-to-procedural-content-generation-algorithms-part-2-random-number-generators-increasing-7811731cea70)