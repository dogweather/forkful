---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Swift: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה & למה?
מחיקת תווים שתואמים הוא תהליך בו משתמשים בתכנות כדי למחוק תווים מאחורי מחרוזת על סמך תבנית מסוימת. תהליך זה חשוב לתיקון ושיפור טקסטים שונים וכן נפוץ בתכנות.

## איך לעשות?
בקוד הבא ניתן לראות דוגמה למחיקת תווים שתואמים בשפת סוויפט:
```Swift
let originalString = "Hello World!"
let newString = originalString.replacingOccurrences(of: "o", with: "")
print(newString) // Prints "Hell Wrld!"
```
בדוגמה זו, אנו משתמשים בפונקציה של שפת סוויפט שנקראת `replacingOccurrences(of:with:)` ומציין בתוכה את התבנית שאנו רוצים למחוק מתוך המחרוזת ואת התו אליו נרצה להחליף את התבנית. הקוד יחזיר את המחרוזת החדשה ללא התווים שהתאימו לתבנית שצוינה.

## עיון עמוק
למחיקת תווים שתואמים יש יישומים מגוונים בתכנות כגון ניקוי מחרוזות, מחיקת תווים ספציפיים מטקסט ועוד. בעבר, בשפות תכנות ישנות יותר כמו פסקל וקובול, המצביעים סימן המחרוזת כדי להצביע על התווים שתרצו למחוק. אולם, בשפת סוויפט ניתן להשתמש בתבניות כדי לסמן את התווים שרוצים למחוק, מה שהופך את התהליך לפשוט וקל להבנה יותר.

## ראו גם
למידע נוסף על מחיקת תווים שתואמים, ניתן לקרוא על זה בכתב העת של סיליקון וואלי, [מחיקת תווים שתואמים בסוויפט](https://www.swift.com/articles/deleting-characters-matching-pattern), וגם בפורום המקוון של סוויפט, [פורום סוויפט עבור מתכנתים](https://forum.swift.org/t/deleting-characters-matching-a-pattern/8539).