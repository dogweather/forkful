---
title:                "יצירת מספרים אקראיים"
html_title:           "TypeScript: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# מה ולמה?
בתכנות, לפעמים אנחנו רוצים ליצור מספרים אקראיים. זה כדי לייצג מידע שונה בכל פעם שהתכנית רצה, או ליצור התנהגות שונה בכל הרצה. כשאתם מריצים תכנית עם מספרים אקראיים, כל פעם שתעצור ותצעד קדימה, תקבלו את תוצאת המספר האקראי אחר.

## איך לעשות?
```TypeScript
//  שימוש בפונקציה Math.random() כדי ליצור מספר אקראי בין 0 ל-1
let randomNum: number = Math.random();
console.log(randomNum);
// תוצאה אפשרית: 0.7284932816192916
```
אם אתם צריכים ליצור מספרים אקראיים בטווח מסוים, תוכלו להשתמש בפונקציה Math.floor() כדי לעגל למספרים שלמים במקום למספרים עשרוניים. למשל, אם אתם רוצים ליצור מספר אקראי בין 1 ל-10:
```TypeScript
let randomInt: number = Math.floor(Math.random() * 10 + 1);
console.log(randomInt);
// תוצאה אפשרית: 7
```

## העמקה
### היסטוריה
יצירת מספרים אקראיים היא תהליך שנעשה מזמן רב כדי לייצג התנהגות אקראית במחשבים. בעבר, קודם לשימוש בטכניקות מתקדמות ביותר כדי ליצור מספרים אקראיים, רוב התכנות השתמש במכשירים כמו קוביה, קופסא עם כדורים, או אפילו כוח בפעלים כדי ליצור מספרים אקראיים.

### אלטרנטיבות
פעמים רבות מתכנתים ישתמשו במספרים אקראיים מסוימים או לפני או במקום הפונקציה Math.random() של TypeScript. למשל, ייתכן שתשתמשו בספרייה חיצונית כמו Lodash או Chance.js עבור פונקציונליות מתקדמת יותר.

### פירוטי המימוש
כדי ליצור מספרים אקראיים, TypeScript משתמשת באלגוריתם המכונה Linear Congruential Generator (אבץ הפכי) המייצר את המספרים האקראיים באופן סדרתי ובכך יוצר בעיות ותבניות בהתנהגות של המספרים.

## ראו גם
- [פירוטי המימוש של Math.random() בקוד דפדוף Chrome](https://v8.dev/blog/math-random)
- [Lodash תיעוד - פעולות רנדומליות](https://lodash.com/docs/#random)
- [תיעוד Chance.js - יצירת מספרים אקראיים](https://chancejs.com/basics/random.html)