---
title:                "Swift: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

ְְ
## למה

אחד הדברים המעצבנים ביותר בתהליך הכתיבה של קוד הוא לחפש ולהחליף טקסט על מנת לשפר את הקוד שלנו. זה יכול להיות מסובך ולקחת זמן רב אם יש הרבה מחרוזות שנדרש לשנות. במאמר זה, אני אראה לכם כיצד ניתן להשתמש בשפת סוויפט כדי לחפש ולהחליף טקסט במהירות ובקלות.

## איך לעשות זאת

השתמשו במתודה `replacingOccurrences(of:with:)` כדי לחפש ולהחליף טקסט. לדוגמה, נדמה שאנחנו רוצים להחליף את המילה "פיתה" במחרוזת "פיצה":

```Swift
let food = "אני אוהב פיתה"
let newFood = food.replacingOccurrences(of: "פיתה", with: "פיצה")
print(newFood) // אני אוהב פיצה
```

## חקירה מעמיקה

בנוסף למתודה `replacingOccurrences(of:with:)`, ישנם עוד כמה אפשרויות לחפש ולהחליף טקסט. למשל, ניתן להשתמש במתודה `replacingOccurrences(of:with:options:range:)` על מנת להגדיר אפשרויות נוספות במהלך החיפוש וההחלפה. ניתן ללמוד עוד על המתודה ותכונותיה בלינקים המצורפים למטה.

## ראו גם

- [למדו עוד על המתודה `replacingOccurrences(of:with:)` כאן](https://developer.apple.com/documentation/foundation/nsstring/1414262-replacingoccurrences)
- [הסבר נרחב יותר על שימוש במתודות עם טקסט בסוויפט](https://www.hackingwithswift.com/quick-start/swiftui/how-to-replace-a-substring)
- [קוד לדוגמה של החיפוש וההחלפה של טקסט בסוויפט](https://stackoverflow.com/questions/45353330/swift-replace-string-within-a-textview/45353484#45353484)