---
title:                "Swift: כותב תכניות למחשב: חיפוש והחלפת טקסט"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה
כתיבת קוד יכולה להיות פעילות מאתגרת, אך לעתים קרובות דברים פשוטים חשבנויות כגון החיפוש וההחלפה של טקסט יכולים לחסוך המון זמן ומתן תוצאות מדויקות.

## כיצד לעשות זאת
כדי לבצע את פעולת החיפוש וההחלפה של טקסט בסוויפט, צריך להשתמש בפונקציה `replacingOccurrences(of:with:)`. לדוגמא, אם נרצה להחליף את המילה "שלום" במשפט למילת "היי", נהפך לעשות כתוב `replacingOccurrences(of: "שלום", with: "היי")`. התוצאה הסופית תהיה "היי, איך לך?".

```
let sentence = "שלום, איך לך?"
let newSentence = sentence.replacingOccurrences(of: "שלום", with: "היי")
print(newSentence)
// היי, איך לך?
```

ניתן להשתמש גם בפונקציה `replacingOccurrences(of:with:options:range:)` כדי לבצע החלפה בטווח מסוים בטקסט.

## מה בעומק
החיפוש וההחלפה של טקסט בסוויפט מבוסס על מודל ה-DOT (Document Object Text). כל מילה, רווח או תו בטקסט הוא אובייקט נפרד במודל זה. בכדי לבצע חיפוש והחלפה של טקסט, הסברט שיטת בדיקת הטקסט והחלפתו באמצעות פילטרים שונים.

## ראו גם
- [מדריך נוסף על חיפוש והחלפה בסוויפט](https://www.hackingwithswift.com/articles/131/how-to-use-regular-expressions-in-swift)
- [מדריך נוסף על מודל DOT (Document Object Text)](https://www.swiftbysundell.com/articles/working-with-strings-in-swift/)
- [מאמר על פונקצית `replacingOccurrences`](https://developer.apple.com/documentation/foundation/nsstring/1414832-replacingoccurrences)