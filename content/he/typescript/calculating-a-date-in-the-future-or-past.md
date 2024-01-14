---
title:                "TypeScript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

בעולם התכנות ישנן הרבה סיבות שיכולות לעורר את התעניינות של מתכנתים בחישוב תאריך בעתיד או בעבר. אחת מהן היא יכולת ליישם תכניות מבוססות תאריך או לעזור למצוא מידע זמני לפי תאריך.

# כיצד לעשות זאת

התכנית הבאה מדגימה כיצד לחשב תאריך בעתיד או בעבר באמצעות שפת תכנות TypeScript:

```TypeScript
// קבל את התאריך הנוכחי
let today = new Date();

// הוסף יום אחד לתאריך נוכחי לחישוב תאריך בעתיד
today.setDate(today.getDate() + 1);

// הדפס את התאריך החדש
console.log(today);

// חישוב תאריך בעבר על ידי חיסור ימים מהתאריך הנוכחי
let pastDate = new Date(today);
pastDate.setDate(today.getDate() - 7);

// הדפס את התאריך בעבר
console.log(pastDate);
```

תוצאת הפלט של התכנית הנ"ל תהיה:

```
Fri Jun 12 2020 18:36:00 GMT+0300 (שעון קיץ ירושלים)
Mon Jun 8 2020 18:36:00 GMT+0300 (שעון קיץ ירושלים)
```

# עיון מעמיק

חישוב תאריך בעתיד או בעבר יכול להיות מועיל בעיצוב ובמיקור חוץ של תכניות מבוססות תאריך או ביצוע חיפושים זמניים. בנוסף, יכולת לחשב תאריך בעתיד או בעבר נדרשת גם במספר תחומים אחרים בתעשיית התוכנה, כגון מערכות ניהול תוכניות ומערכות ניהול פרויקטים.

# ראה גם

- [תיעוד נוסף לגבי חישוב תאריך ב-TypeScript](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [אתר חישוב תאריכים לעזרה במחשבון תאריכים מתקדם](https://www.timeanddate.com/date/dateadded.html)