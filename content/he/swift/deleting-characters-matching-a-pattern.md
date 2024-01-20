---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
מחיקת תווים שמתאימים לתבנית היא תהליך שבו המתכנת מסיר תווים מסוימים מתוך מחרוזת, בהתאם לקריטריונים כלשהם. הסיבה שלנו כמתכנתים לביצוע אופרציה זו היא לנקות מחרוזת, כדי להביא אותה לצורה הרצויה

## איך לעשות:
כדי למחוק תווים שמתאימים לתבנית ב-Swift, אנחנו יכולים לשנות את המחרוזת שלנו עם `replacingOccurrences`. נראה דוגמה:

```swift
var str = "Hello, Playground"
str = str.replacingOccurrences(of: "[aeiou]", with: "", options: .regularExpression)
print(str)
```

בדוגמאות אלה, כל התווים "[aeiou]" - העיצובים - מוחקים מהמחרוזת.
זה ידפיס: "Hll, Plygrnd".

## בעומק:
פעולת מחיקת התווים שמתאימים לתבנית הייתה כאן מאז תחילת תכנות המחשבים והיא נמצאת ברוב שפות התכנות. 

חלופה ל `replacingOccurrences` היא השימוש בפונקציה `reduce` עם קודם ראשוני כדי לבניית מחרוזת שהיא אותה מחרוזת ללא התווים שמתאימים לתבנית.

אף על פי שהפונקציה `replacingOccurrences` מאפשרת לנו למחוק תווים שמתאימים לתבנית, אנו עדיין צריכים מערך של תווים להשוואה.

## ראה גם:
ללמוד עוד על טיפול במחרוזות ב-Swift, בדוק את המדריך של המפתחים של Apple [כאן] (https://developer.apple.com/documentation/swift/string) והמאמר של התכנית נויפה לימוד עצמי [כאן](https://www.hackingwithswift.com/read/0/2/strings-and-variables) על מחרוזות ומשתנים.