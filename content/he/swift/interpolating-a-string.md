---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת היא הטמעת נתונים (כמו משתנים) בתוך מחרוזת. מתכנתים עושים זאת כדי להקליט או להציג מידע יחד עם הודעות טקסט.

## איך:
הנה קטע קוד שמדגים כיצד לערבב מחרוזת:
```Swift 
let name = "Dan"
print("Hello, \(name)!")
```
הפלט של הקטע הזה לדוגמא יהיה: `Hello, Dan!`

## צלילה עמוקה:
אינטרפולציה היא מאפיין מרכזי בשפות תכנות רבות. אלטרנטיבה לאינטרפולציה היא שימוש בפונקציה `concat` לשם התקנה של מחרוזות - אך זו אפשרות פחות קלה לקריאה ותרגול. ב-Swift, המערכת מנהלת זיכרון של נתוני המחרוזת שהוטמעו באינטרפולציה, כך שאין צורך לדאוג על ניהול זיכרון מיוחד.

## ראו גם:
* [מדריך Apple ל-A Swift](https://developer.apple.com/swift/resources/)
* [מאמר עמוק יותר על אינטרפולציה של מחרוזת ב-Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5-0)