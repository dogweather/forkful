---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיבוד מחרוזות הוא תהליך של שליפת מספר מחדילים של מחרוזות ושלבהן למחרוזת אחת. מתכנתים עושים זאת כדי ליצור טקסטים מורכבים (כגון הודעות, דפי HTML) באופן דינמי.

## איך לעשות:
הנה דרך לבצע עיבוד מחרוזות בשפת Swift:

```Swift
var string1 = "היי, "
var string2 = "איך אני יכול לעזור לך?"
var message = string1 + string2
print(message)
```

אתה צפוי לראות את הפלט הבא:

```Swift
היי, איך אני כול לעזור לך?
```

## צלילה עמוקה
עיבוד מחרוזות הוא לא מורכב, אבל זה זכאי לכמה הזכרה של ההיסטוריה והאלטרנטיבות שלו. אף פעם לא נכתב במקור בשפות מהמדתיות, אבל Swift בנוי כל כך כמו שאפשר לך ליצור בקלות חלקים מנותקים של טקסט. ייתכן ותבחר לשנות משתנים, להשתמש במחרוזות תבנית או אף לחלק מחרוזת למערך.

## ראה גם:
* הדרכות של Apple למחרוזות ב-Swift: https://developer.apple.com/documentation/swift/string
* דף Wikipedia של Python: https://he.wikipedia.org/wiki/Swift_(שפת_תוכנה)
* בלוג Hackernoon על עיבוד מחרוזות ב-Swift: https://hackernoon.com/swift-strings-6fc2629aecf7