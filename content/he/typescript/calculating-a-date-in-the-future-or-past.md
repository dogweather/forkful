---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "TypeScript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

קבלת תאריך בעתיד או בעבר יכול להיות שימושי במגוון מקרים, כגון יישומי תאריכים או לתכנות טיסות ואירועים. באמצעות תוכנת TypeScript, ניתן לבנות כלי שיכול לחשב את התאריך בדיוק כפי שאנו צריכים.

## איך לעשות זאת

הנה כמה דרכים שבאמצעותן אנו יכולים לחשב תאריך בעבר או בעתיד ב TypeScript:

```TypeScript
// חישוב תאריך בעתיד
const currentDate = new Date(); // מציג את התאריך הנוכחי
const futureDate = new Date(); // יציג את התאריך התקווה
futureDate.setDate(currentDate.getDate() + 7); // מוסיף 7 ימים לתאריך הנוכחי
console.log(futureDate); // יציג את התאריך החדש

// חישוב תאריך בעבר
const pastDate = new Date(); // מציג את התאריך הנוכחי
pastDate.setDate(currentDate.getDate() - 7); // מחזיר את התאריך שלפני 7 ימים מהתאריך הנוכחי
console.log(pastDate); // יציג את התאריך החדש
```

החישובים הללו משתמשים בפונקציות חדשות באובייקט התאריך של JavaScript, כך שזה יכול להיות מעט מעכב בהתחלה. זה חשוב לוודא שאנו מפעילים את הפונקציה `new Date()` לפני שאנו מנסים להשתמש בכל פונקציות תאריך אחרות.

## חידוד

כדי לחשב תאריך בדיוק בעתיד או בעבר, יש כמה דברים שיש לקחת בחשבון. כשאנו משנים את תאריך היום, זה משנה גם את הימים והחודשים המתאימים. לכן, חשוב לוודא שאנו משתמשים בפונקציות הנכונות כדי להפעיל את התאריך המדויק שאנו מעוניינים בו.

בנוסף, גם יש לקחת בחשבון כי ישנם חודשים עם מספר ימים שונים. לדוגמה, פברואר יכול להי