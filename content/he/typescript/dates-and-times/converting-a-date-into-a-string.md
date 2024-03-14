---
date: 2024-01-20 17:38:07.483588-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E9\u05D5\u05DE\u05E8\u05D9\
  \u05DD \u05D0\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\
  \u05E1\u05D8, \u05DB\u05D3\u05D9 \u05E9\u05E0\u05D5\u05DB\u05DC \u05DC\u05D4\u05E6\
  \u05D9\u05D2 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\
  \u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF\
  \ \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05DE\u05D0\u05D5\u05D7\u05E8 \u05D9\u05D5\
  \u05EA\u05E8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.936579-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E9\u05D5\u05DE\u05E8\u05D9\u05DD\
  \ \u05D0\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D4\u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05D8\u05E7\u05E1\
  \u05D8, \u05DB\u05D3\u05D9 \u05E9\u05E0\u05D5\u05DB\u05DC \u05DC\u05D4\u05E6\u05D9\
  \u05D2 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD\
  \ \u05E9\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05D0\u05D7\u05E1\u05DF \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05DE\u05D0\u05D5\u05D7\u05E8 \u05D9\u05D5\u05EA\
  \u05E8. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא תהליך שבו אנחנו שומרים את הנתונים של התאריך בפורמט טקסט, כדי שנוכל להציג אותו בפורמטים שונים או לאחסן לשימוש מאוחר יותר. מתכנתים עושים זאת כי לא כל מערכת יכולה לקרוא את פורמט התאריך המקורי, ולעיתים צריך להעביר את התאריך בין שפות תכנות או מערכות.

## איך לעשות:
```typescript
// ייצוא תאריך כרגע בפורמט מחרוזת סטנדרטי (ISO)
const currentDate: Date = new Date();
const dateAsString: string = currentDate.toISOString();
console.log(dateAsString); // דוגמת פלט: "2023-03-16T15:20:30.045Z"

// המרת תאריך למחרוזת בפורמט מקומי
const localDateString: string = currentDate.toLocaleDateString('he-IL');
console.log(localDateString); // דוגמת פלט: "16.3.2023"

// המרת תאריך למחרוזת עם זמן מקומי
const localTimeString: string = currentDate.toLocaleTimeString('he-IL');
console.log(localTimeString); // דוגמת פלט: "18:20:30"
```

## צלילה לעומק
בעבר, המרת תאריך למחרוזת בעלת פורמט ספציפי הייתה תהליך מסובך יותר שדרש כתיבת קוד ארוך ופחות יעיל. עם הזמן, תקנים חדשים וכלים מודרניים כמו ECMAScript(Internationalization API) הובילו לכך שהמרה זו הפכה לפשוטה ונוחה יותר. פונקציות כמו `toLocaleDateString` ו-`toLocaleTimeString` מאפשרות למתכנתים להמיר תאריכים למחרוזות ללא צורך בקוד נפרד לפרמוט התאריכים. ישנם אלטרנטיבות רבות להמרת תאריכים, כמו ספריות חיצוניות (לדוגמה, `moment.js` ו`date-fns`) שמספקות גמישות נוספת בעיצוב תאריכים.

השתמש ב-API הבינלאומי כדי להתאים את הפורמט לשפה ולאזור שלך, במקום להיצמד לפורמטים סטטיים שלא תמיד יובנו על ידי המשתמש הקצה.

## ראה גם
- [MDN Web Docs - Date.prototype.toISOString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)
- [MDN Web Docs - Date.prototype.toLocaleDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN Web Docs - Date.prototype.toLocaleTimeString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString)
- [ספריית moment.js](https://momentjs.com/)
- [ספריית date-fns](https://date-fns.org/)
