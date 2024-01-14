---
title:    "TypeScript: חישוב תאריך בעתיד או בעבר"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

חישוב תאריך בעתיד או בעבר הוא כלי חשוב בתכנות בטיפוסקריפט. זה מאפשר לנו להגדיר תאריך מסוים ולפעמים אנו רוצים לחשב את התאריך שמתקיים למעבר מספר ימים או רבעות בעבר או בעתיד. ועם כלי זה, אנחנו יכולים למצוא תאריך נתון במקום להזין ידנית כל פעם.

## איך לעשות

תחילה עלינו להגדיר את התאריך הנוכחי בטיפוסקריפט על ידי השמתו למשתנה `currentDate`. נשתמש באובייקט `Date` כדי ליצור את התאריך הנוכחי:

```TypeScript
let currentDate: Date = new Date();
```

עכשיו, נרצה לחשב תאריך שבו נמצא אחרי מספר ימים מהתאריך הנוכחי. נוסיף פרמטר כמספר ימים לפונקציה `getDate()` ונחשב את התאריך החדש על ידי הוספת המספר של ימים לתאריך הנוכחי:
```TypeScript
function getDate(days: number): Date {
    let newDate: Date = new Date();
    newDate.setDate(currentDate.getDate() + days);
    return newDate;
}
let futureDate: Date = getDate(10);
console.log(futureDate);
```

יצא:
`Mon Jun 22 2020 20:33:53 GMT+0300 (Israel Daylight Time)`

עם הפונקציה הזאת, אנחנו יכולים לחשב תאריך מסוים בעתיד או בעבר בהתאם למספר הימים שנמצא מהתאריך הנוכחי.

## Deep Dive

כשנמצא שעברנו מספר ימים לתאריך מסוים, הרבה פעמים נרצה להדפיס את התאריך בפורמט נכון. ניתן לעשות זאת על ידי שימוש בפונקציות מובנות ב- `Date` כמו `toISOString()`, `toLocaleDateString()` ועוד.

בנוסף, ישנם כלים נוספים בטיפוסקריפט שמאפשרים חישוב תאריכים וזמנים ועיבוד מידע באופן יעיל יותר, כגון התוסף הפופולרי [Moment.js](https://momentjs.com/