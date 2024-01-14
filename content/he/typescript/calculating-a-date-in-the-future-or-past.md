---
title:    "TypeScript: חישוב תאריך בעתיד או בעבר"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה

שימוש בתכנות TypeScript עבור חישוב תאריך בעתיד או בעבר נעשה על מנת ליצור קוד גמיש ומאורגן, שניתן להשתמש בו עבור בתי עסק מגוונים ופרויקטים. חישוב תאריך יכול להיות מועיל לשם תחזוקת תאריכים עסקיים או אישיים, יצירת יומנים או יצירת תאריכים לטלפונים ניידים.

## איך לעשות זאת

במקרה הכללי, לחישוב תאריך בעתיד או בעבר ניתן להשתמש בטכניקות פשוטות כמו חיבור וכפל של מספרים. לדוגמה, ננסה להוסיף 5 ימים לתאריך הנוכחי:

```TypeScript
let currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 5);

console.log(currentDate.toDateString()); // תוצאה: Thu Aug 05 2021
```

ניתן גם להשתמש בספרייה שליצרה גוגל, moment.js, המציעה פונקציות מתקדמות יותר לתאריכים ושעות. ניתן להתקין את הספרייה דרך פקד npm ולהשתמש בה כדי לחשב תאריכים יותר מדויקים ומיוחדים.

```TypeScript
import * as moment from 'moment';

let dateToCalculate = moment().add(5, 'days').format('MMMM Do YYYY');

console.log(dateToCalculate); // תוצאה: August 5th 2021
```

כמו כן, ניתן להשתמש בפונקציות ייחודיות כמו חישוב תאריך היום הראשון של חודש מסוים בעתיד או בעבר.

```TypeScript
let firstDayOfMonth = moment().startOf('month').add(1, 'days').format('DD/MM/YYYY');
console.log(firstDayOfMonth); // תוצאה:  02/08/2021 שניים באוגוסט 2021
```

## עיון מעמיק

כאשר מתארים ומחשבים תאריכים בשפת TypeScript, חשוב לקחת בחשבון הפעמים כמו פעמים מעורפלים, המוני יחידה זיכרון וקוד דפוק בסיסי בזמן ריצת הקוד. קריאת התיעוד של תכנון הספריות,