---
date: 2024-01-20 17:46:15.934882-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05E7\u05D7\
  \u05EA \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D2\u05D3\
  \u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5\
  \ \u05D1\u05E9\u05D1\u05D9\u05DC \u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05D5\u05DC\u05D9\u05D3\u05E6\u05D9\u05D5\u05EA, \u05D5\u05DB\
  \u05E9\u05D0\u05E0\u05D7\u05E0\u05D5 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05D4\
  \u05E6\u05D9\u05D2 \u05E8\u05E7 \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05DB\u05DC\u05E9\u05D4\u05D9 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9\
  ."
lastmod: '2024-03-13T22:44:39.956965-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DA \u05DC\u05E7\u05D7\u05EA\
  \ \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D2\u05D3\u05D5\
  \u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5 \u05D1\
  \u05E9\u05D1\u05D9\u05DC \u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05D5\u05DC\u05D9\u05D3\u05E6\u05D9\u05D5\u05EA, \u05D5\u05DB\u05E9\
  \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05D4\u05E6\
  \u05D9\u05D2 \u05E8\u05E7 \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DB\u05DC\u05E9\u05D4\u05D9 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות מאפשר לך לקחת חלק ממחרוזת גדולה יותר. זה נחוץ בשביל עיבוד נתונים, ולידציות, וכשאנחנו רוצים להציג רק חלק ממחרוזת כלשהי למשתמש.

## איך לעשות:
```Javascript
let fullString = "שלום, עולם!";
let partOfString = fullString.substring(0, 5); // חילוץ התווים מתחילת המחרוזת עד האינדקס 5
console.log(partOfString); // תוצאה: שלום,

let anotherPart = fullString.slice(-6); // חילוץ 6 התווים האחרונים
console.log(anotherPart); // תוצאה: עולם!
```

## טבילה עמוקה:
בעבר, ב-JavaScript הישן, היינו משתמשים ב-`.substr()` כדי לחלץ תת-מחרוזות, אבל היום מומלץ להשתמש ב-`.substring()` או ב-`.slice()`. כשאתה משתמש ב-`.substring()`, אם האינדקס הראשון גבוה יותר מהשני, הם מתחלפים. אם ב-`.slice()` האינדקס הראשון יהיה גבוה יותר, תחזור מחרוזת ריקה. בנוסף, `.slice()` תומך באינדקסים שליליים עבור חילוץ סופי.

## ראו גם:
- [דוקומנטציה של String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [דוקומנטציה של String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
