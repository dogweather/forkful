---
title:                "TypeScript: קבלת התאריך הנוכחי"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה
מחשב אתרים משמשים לפעמים כדי להציג את התאריך הנוכחי למשתמשים שמשתמשים באתר או באפליקציה. זה יכול להיות מועיל לצורך ניתוח נתונים או לתכנון תאריכי תאריך ושעה לאירועים מסוימים.

## איך לעשות זאת
ניתן לקבל את התאריך הנוכחי באמצעות ספריית השפת JavaScript הפופולרית, Date. תחילה ניצור משנת מסוג Date עם הצהרת טיפוס ונתוני התאריך הנוכחי. למשל:

```TypeScript
let currentDate: Date = new Date();
```

לאחר מכן, נוכל להשתמש בשיטות נוספות של ספריית Date כדי להשיג מידע מדוייק יותר על התאריך הנוכחי. למשל, נוכל להשתמש בשיטות כמו getDate(), getDay(), getFullYear(), ועוד כדי לקבל מידע על יום החודש, השבוע, השנה וכו'.

לדוגמה, אם נרצה להציג את התאריך הנוכחי בפורמט של MM/DD/YYYY, נשתמש בקוד הבא:

```TypeScript
let currentDate: Date = new Date();
let month: number = currentDate.getMonth() + 1; // להוסיף 1 כי החודשים מתחילים מ-0
let day: number = currentDate.getDate();
let year: number = currentDate.getFullYear();
console.log(`${month}/${day}/${year}`);
```

פלט:

```
9/10/2021
```

## עיון מעמיק
השפת JavaScript מגדירה Date כאובייקט המייצג תאריך ושעה. התאריך מיוצג על ידי מספר גדול המייצג כמה מילי-שניות חלפו מאז ה-01/01/1970 בשעה 00:00:00 (קואורדינטת גריניץ'). כל כך הרבה משתמשים מתמודדים עם זה, כך שכמה ספריות אחרות נוצרו על מנת להתעסק עם תאריכים ושעות בפורמטים מקובלים יותר.

ספריית Moment היא דוגמה טובה לכך, היא מציעה מגוון שיטות ופונקציות נוספות לצורך ה