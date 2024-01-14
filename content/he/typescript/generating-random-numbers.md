---
title:                "TypeScript: יצירת מספרים אקראיים"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה

כתיבת קוד טיפוסקריפט מדורג מהספריה "Math" היא כלי מצוין ליצירת מספרים רנדומליים. זה יכול להיות שימושי לאימות, מבחנים או גרפיים. זה גם משמש ככלי ליצירת פעוטות יישומיים כמו משחקי מחשב או הגרלות מקרים.

## איך לעשות זאת

אם אתם מחפשים כתיבת קוד בסיסית שיכולה לייצר מספרים רנדומליים, כאן יש כמה דוגמאות שאפשר לעקוב אחריהם.

```TypeScript
// ייבוא מהספריה של Math
import { Math } from 'math';

//  הפונקציה "random" תחזיר מספר רנדומלי בין 0 ל-1
let randomNumber: number = Math.random();
console.log(randomNumber);

// כדי לקבל מספר רנדומלי בין 0 ל-100, נדגל לפעולה "floor" פנימה
let randomRange: number = Math.floor(Math.random() * 100);
console.log(randomRange);
```

פלט:
```
0.3475789549785
54
```

ולהלן דוגמא נוספת עם שימוש בספריית "lodash", כמו בברסר מכונת מצב:

```TypeScript 
// ייבוא מהספריה של "lodash"
import _ from 'lodash';

// פעולה זו תחזיר לנו מספר רנדומלי בטווח על-הגבלה
let randomNonNegative: number = _.random(0, 100, true);
console.log(randomNonNegative);

// עכשיו נדגל לפעולה
let randomNegaive: number = _.random(-100, 0, true);
console.log(randomNegative);
```

פלט:
```
38.56476987
-85.467689
```

## חפירה רחבה

מהו בדיוק מספר רנדומלי? זה מספר שנבחר מתוך טווח כלשהו באופן בלתי צפוי. בקוד טיפוסקריפט, פעולות המתייחסות לאובייקט "Math" מייצגות את הספריה המובנית של פעולות מתמטיות וחישוביות. באמצעות הפעולה "random", אנחנו יכולים ליצור מספרים רנדומליים בטווח שנרצה. יש לציין שהמ