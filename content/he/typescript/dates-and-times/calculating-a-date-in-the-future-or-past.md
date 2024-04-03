---
date: 2024-01-20 17:33:01.662869-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: TypeScript \u05DB\
  \u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\u05D1\u05D9\u05DC\u05EA `Date`, \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\u05D7\u05E9\u05D1 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5\
  \ \u05D1\u05E2\u05D1\u05E8 \u05D1\u05E7\u05DC\u05D5\u05EA. \u05D3\u05D5\u05D2\u05DE\
  \u05D4."
lastmod: '2024-03-13T22:44:38.939878-06:00'
model: gpt-4-1106-preview
summary: "TypeScript \u05DB\u05D5\u05DC\u05DC \u05D0\u05EA \u05D7\u05D1\u05D9\u05DC\
  \u05EA `Date`, \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\
  \u05D7\u05E9\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1\u05E2\u05EA\
  \u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D1\u05E7\u05DC\u05D5\u05EA\
  ."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
TypeScript כולל את חבילת `Date`, שמאפשרת לנו לחשב תאריכים בעתיד או בעבר בקלות. דוגמה:

```TypeScript
// הוספת 10 ימים לתאריך הנוכחי
const today: Date = new Date();
const tenDaysLater: Date = new Date(today);
tenDaysLater.setDate(tenDaysLater.getDate() + 10);
console.log(tenDaysLater.toLocaleDateString()); // תצוגת התאריך החדש

// חיסור של 5 שנים מתאריך ספציפי
const specificDate: Date = new Date('2023-04-01');
const fiveYearsEarlier: Date = new Date(specificDate);
fiveYearsEarlier.setFullYear(fiveYearsEarlier.getFullYear() - 5);
console.log(fiveYearsEarlier.toLocaleDateString()); // תצוגת התאריך החדש
```

תוצאה:
```
21/04/2023 // או מה שיחול על יום ההדפסה, 10 ימים אחרי
01/04/2018 // תמיד יהיה תוצאה זהה
```

## צלילה לעומק:
בעבר, חישובי תאריכים נעשו בצורה ידנית ולעיתים גם שגויים בגלל חולשת החישובים ומורכבויות של לוחות השנה. בזמן המודרני, מחלקות זמן כמו `Date` ב-JavaScript ו-TypeScript מאפשרות לנו לבצע את הפעולות הללו עם דיוק וקלות.

ישנם גם ספריות נוספות כמו `moment.js` או `date-fns` שמספקות עוד יותר פונקציונליות ונוחות. לפעמים הם מועדפים לשימוש בשל נקודות תורפה בממשק ה-`Date` המקורי, כמו טיפול באיזורי זמן שונים ופורמטים מורכבים של תאריכים.

כשאנחנו עובדים עם `Date`, חשוב לזכור שחודשים מתחילים מ-0 (ינואר) עד 11 (דצמבר) ושיש לשים לב לזמן עולמי מתואם (UTC) בשעה שמעבדים תאריכים במערכות מבוזרות.

הבעיה הנפוצה ביותר בעבודה עם האובייקט `Date` היא התייחסות לשנים מעוברות ולשינויי זמן (DST – Daylight Saving Time). כאשר אנו מחשבים תאריכים לעתיד או לעבר, יש לבדוק תמיד שהתאריך החדש אינו נתון לשגיאות הנובעות מגורמים אלו.

## ראה גם:
1. המדריך המלא ל-Date ב-Mozilla Developer Network: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
2. Moment.js, ספריית תאריכים מובילה: https://momentjs.com/
3. date-fns, ספרייה עדכנית לפעולות עם תאריכים: https://date-fns.org/
