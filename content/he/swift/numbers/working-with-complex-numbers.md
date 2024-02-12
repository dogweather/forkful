---
title:                "עבודה עם מספרים מרוכבים"
aliases:
- he/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:46:14.985704-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מכילים חלק ממשי וחלק מדומה (כמו 3 + 4i). תכנתים משתמשים בהם ב-Swift למטלות כמו עיבוד אותות, פתרון סוגים מסוימים של בעיות מתמטיות, וסימולציות פיזיקליות.

## איך ל:
Swift אינו כולל תמיכה מובנית במספרים מרוכבים, אך אנו יכולים ליצור אחד משלנו:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // שיטות נוספות כמו חיסור, כפל, וכו'.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("תוצאה: \(result.real) + \(result.imaginary)i")
// פלט לדוגמה: תוצאה: 3.0 + 7.0i
```

## צלילה עמוקה
מספרים מרוכבים צצו במאה ה-16 במשוואות אלגבריות. הם חיוניים במכניקת הקוונטים, תורת הבקרה, ותחומים רבים נוספים. Swift של אפל אינו כולל ספרייה סטנדרטית למספרים מרוכבים, בניגוד לשפות כמו Python או C++. אלטרנטיבות ליצירה משלך כוללות שימוש בחבילת Numerics, הכוללת תמיכה במספרים מרוכבים, או העטפת ספריית המספרים המרוכבים של C++ עם האינטרופרביליות של Swift.

## ראה גם
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
