---
title:                "השוואת שני תאריכים"
html_title:           "Javascript: השוואת שני תאריכים"
simple_title:         "השוואת שני תאריכים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

השוואה של שתי תאריכים היא תהליך שבו מתבצעת בדיקה של המערכת שלנו לגבי איזה תאריכים הם אחידים זה עם זה. פעמים רבות תאריכים הם תלויים ביום העכשיוי שבו הם נכתבים, וכך קורה שבשתי מערכות שונות תאריכים נראים כמו שונים. מכאן, נובעת הצורך לבצע שוואה בין תאריכים כדי להבטיח תאימות בין מערכות שונות.

## איך לעשות זאת:

למטה תמצאו כמה דוגמאות של קוד בג'אווהסקריפט לביצוע שוואה של שתי תאריכים. הקוד יראה לכם כיצד להשתמש בפונקציות המיועדות לכך ותראו את הפלט שהוא יחזיר. זה יעזור לכם להבין טוב יותר את התהליך ולהיות בטוחים שאתם מבינים אותו בצורה נכונה.

```Javascript
// התאריך הנוכחי בתור אובייקט
var today = new Date();

// פונקציית לייחוז המחזירה את התאריך הנוכחי כטקסט ומחיליץ ממנו רק את החודש והשנה
function getMonthAndYear(date) {
  var month = date.getMonth();
  var year = date.getFullYear();
  
  return month + " " + year;
}

var currentDate = getMonthAndYear(today);
console.log("זהו התאריך הנוכחי שנמצא באובייקט של today: " + currentDate);

// פונקציית שוואה של שני תאריכים
function compareDates(date1, date2) {
  if (date1.getTime() > date2.getTime()) {
    console.log(date1 + " הוא שני תאריך המוקדם יותר");
  } else if (date1.getTime() < date2.getTime()) {
    console.log(date2 + " הוא שני תאריך המוקדם יותר");
  } else {
    console.log("שני התאריכים שווים");
  }
}

// התאריך הנוכחי יוצא כאילו הוא הגדרה של אותו חודש ואותה שנה, אז אנחנו מצפים שתקבלו הודעת שני התאריכים שווים
compareDates(today, new Date(2020, 5, 12));

```

## מעולה, מתי נבדוק את זה בפועל?

אם אתם מעוניינים להכיר יותר בעומק את הנושא, תוכלו להתעמק בידע נוסף על הסטוריות שונות של שני התאריכים, פונקציות שניתנות לשימוש ועוד אלטרנטיבות לבודקי תאריכים בג'אווהסקריפט. כמו כן, כדאי לעיין בפרסומים השונים הקשורים לנושא כדי להשתלב בהם בתוכניות שלכם.

## ראו גם:

[הספריה Moment.js](https://momentjs.com/)

[בדיקת תאריך בג'אווהסקריפט](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/Date)