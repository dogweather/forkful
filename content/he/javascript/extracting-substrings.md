---
title:                "חילוץ תת-מחרוזות"
aliases:
- he/javascript/extracting-substrings.md
date:                  2024-01-20T17:46:15.934882-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/extracting-substrings.md"
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
