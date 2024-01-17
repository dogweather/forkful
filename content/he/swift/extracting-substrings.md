---
title:                "חילוץ תת-מחרוזות"
html_title:           "Swift: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
קיצור מסדרת אותיות הוא תהליך של ניתוח נתונים טקסטואליים כדי למצוא חלקים מסוימים של טקסט. תוכניתאים עושים זאת כדי לעבוד עם מידע ספציפי ולהפשיט את הנתון לפי הצורך.

## כיצד לעשות זאת: 
להלן כמה דוגמאות עם מידע ופלט לתוכנת Swift ... 
```Swift
let sentence = "שלום! אני תוכנית סוויפט." 
let substring = sentence[17..<24] 

print(substring) // תוכנית
```

```Swift 
let text = "מעוניינים ללמוד סוויפט? הצטרפו עכשיו!" 
let part = text[16..<20] 

print(part) // סוויפט
```

```Swift 
let name = "יוסף" 
let firstName = name[0..<2] 

print(firstName) // יו
```

## נכנסים לתהליך: 
קיצור מסדרת אותיות משמש כלי מאוד עוצמתי בתכנות, ומשתמשים בו מזה שנות ה- 1950. זה אופציה נוחה לעבוד עם טקסטים ולחלץ אינפורמציה מסוימת. ישנם גם אפשרויות אחרות כמו החיתוך של מחרוזות, אך קיצור מסדרת אותיות מספק אפשרויות מתקדמות יותר ומאפשר למנוע שגיאות.

## ראה גם:
- [מדריך מלא על קיצור מסדרת אותיות ב-Swift](https://www.appcoda.com/swift-string-substring/)
- [תיעוד רשמי של Swift על קיצור מסדרת אותיות](https://developer.apple.com/documentation/swift/string/1642959-substring)