---
title:                "Javascript: חישוב תאריך בעתיד או בעבר"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מדוע

למה לזהות זמן בעתיד או בעבר באמצעות תכנות ב-Javascript? אחד הסיבות העיקריות לכך הוא לתכנת תאריכי הפעילות או אירועים עתידיים לקבוע מדויק מתי הם קורים.

## איך לעשות

על מנת לחשב תאריך בעבר או בעתיד, נשתמש באובייקט המובנה של Javascript הנקרא `Date`. כדי ליצור אובייקט תאריך חדש, נשתמש בפונקציית הבנאי `new Date()` ונעביר לה את הפרמטרים המתאימים לתאריך הרצוי. הנה כמה דוגמאות של קוד ופלט:

```Javascript
// קביעת תאריך של יום שלישי הבא
const nextTuesday = new Date('2021-11-30');
console.log(nextTuesday); // Output: Tue Nov 30 2021 00:00:00 GMT+0200 (Eastern European Standard Time)

// קביעת תאריך של שנה לפני היום הנוכחי
const lastYear = new Date(2020, 10, 23);
console.log(lastYear); // Output: Mon Nov 23 2020 00:00:00 GMT+0200 (Eastern European Standard Time)
```

ניתן גם להשתמש בפונקציות של האובייקט `Date`, כגון `getDate()`, `getMonth()`, ו-`getFullYear()`, כדי לגשת למידע ספציפי על תאריך מסוים. לדוגמה, ניתן להשתמש בפונקציה `getMonth()` כדי לקבל את שם החודש עבור תאריך מסוים. הנה דוגמא נוספת:

```Javascript
const date = new Date('2021-12-25');

// השגת שם החודש והיום בחודש
console.log(date.getMonth()); // Output: 11 (המיקום של חודש דצמבר במערך החודשים ב-Javascript)
console.log(date.getDate()); // Output: 25

// השתמשות בתאריך לקבלת פלט נוח יותר
console.log(`${date.getDate()}/${date.getMonth() + 1}/${date.getFullYear()}`); // Output: 25/12/2021
```

## Deep Dive

אובייקט התאריך ב-Javascript מאפשר לנו לכלול בתוכו מידע נרחב על תאריך מסוים, כגון יום בשבוע, יום בחודש, שעה, ועוד. בנוסף, ניתן להשתמש בפונקציות כמו `setDate()` ו-`setFullYear()`