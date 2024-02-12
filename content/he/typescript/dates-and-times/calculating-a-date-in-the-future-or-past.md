---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- /he/typescript/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:33:01.662869-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה שבה מקבלים תאריך ומוסיפים או מחסירים ממנו זמן כדי לקבל תאריך חדש. תכנתים עושים את זה למטרות שונות, כמו לתזמן אירועים, ליצור לוחות זמנים או לחשב מועדי פקיעה.

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
