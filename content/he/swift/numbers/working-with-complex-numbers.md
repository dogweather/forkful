---
date: 2024-01-26 04:46:14.985704-07:00
description: "\u05D0\u05D9\u05DA \u05DC: Swift \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\
  \u05DC\u05DC \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D0\u05DA \u05D0\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\
  \u05D9\u05E6\u05D5\u05E8 \u05D0\u05D7\u05D3 \u05DE\u05E9\u05DC\u05E0\u05D5."
lastmod: '2024-03-13T22:44:39.895587-06:00'
model: gpt-4-0125-preview
summary: "Swift \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05EA\u05DE\u05D9\
  \u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D1\u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD, \u05D0\u05DA \u05D0\u05E0\
  \u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D0\
  \u05D7\u05D3 \u05DE\u05E9\u05DC\u05E0\u05D5."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

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
