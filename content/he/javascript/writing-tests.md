---
title:                "כתיבת מבחנים"
html_title:           "Javascript: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-tests.md"
---

{{< edit_this_page >}}

מה ולמה?

כתיבת בדיקות היא תהליך שבו מתכנתים בוחנים את הקוד שלהם על מנת לוודא שהוא עובד כמצופה. תהליך זה מאפשר למתכנתים לזהות בעיות ולתקן אותן בטרם יימצאו בשייכות מציגת הקוד למשתמשים. מתכנתים מתחשקים לעשות זאת כי הם רוצים שהמוצר שלהם יהיה באיכות הגבוהה ביותר ולא מעולם לעמוד בפני תקלות בעת המציגה.

איך לעשות את זה:

```Javascript
// דוגמא
function add(a, b) {
  return a + b;
}

console.log(add(3, 5));
// פלט: 8
```

לעמוד לעומק:

לפני השנות ה-90, כתיבת בדיקות הייתה תהליך מפרכס שממשיך עד היום. כיום ישנן כלים רבים שעוזרים למתכנתים לכתוב בדיקות בצורה יעילה יותר ולהחזיק אותן מתוכנתות. אחת מהכלים הפופולריים ביותר כיום היא Jest, המאפשר פעולות ובדיקות מאוחרות ביחד באופן מלאות.

ראה גם:

- [הדרכה על Jest](https://jestjs.io/docs/getting-started)
- [למידע נוסף על כתיבת בדיקות ב-Javascript](https://www.telerik.com/blogs/what-is-javascript-unit-testing)