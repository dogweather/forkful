---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:42.850753-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Swift \u05EA\u05D5\
  \u05DE\u05DA \u05D1\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D3\u05E8\u05DA \u05D4\
  \u05DE\u05E1\u05D2\u05E8\u05EA XCTest, \u05D0\u05E9\u05E8 \u05DE\u05E9\u05D5\u05DC\
  \u05D1\u05EA \u05D1-Xcode. \u05EA\u05D5\u05DB\u05DC \u05DC\u05DB\u05EA\u05D5\u05D1\
  \ \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D9\u05D7\u05D9\u05D3\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05DE\u05EA \u05D7\u05DC\u05E7\u05D9\u05DD \u05D9\u05D7\u05D9\
  \u05D3\u05E0\u05D9\u05D9\u05DD \u05E9\u05DC \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\
  \u05DA, \u05DC\u05DE\u05E9\u05DC, \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E9\
  \u05DE\u05D7\u05E9\u05D1\u05EA \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.911614-06:00'
model: gpt-4-0125-preview
summary: "Swift \u05EA\u05D5\u05DE\u05DA \u05D1\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D3\u05E8\u05DA \u05D4\u05DE\u05E1\u05D2\u05E8\u05EA XCTest, \u05D0\u05E9\u05E8\
  \ \u05DE\u05E9\u05D5\u05DC\u05D1\u05EA \u05D1-Xcode."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לעשות:
Swift תומך בבדיקות דרך המסגרת XCTest, אשר משולבת ב-Xcode. תוכל לכתוב בדיקות יחידה כדי לאמת חלקים יחידניים של הקוד שלך, למשל, פונקציה שמחשבת את סכום שני מספרים.

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "הפונקציה לחישוב הסכום לא החזירה את הערך הצפוי.")
    }
}
```

כדי להריץ בדיקה זו, אתה בדרך כלל תלחץ Command-U ב-Xcode. הפלט בנווט הבדיקות של Xcode יגיד לך אם הבדיקה עברה או נכשלה.

לדוגמה, פלט של בדיקה מוצלחת:
```
נפלא '-[YourAppTests testSum]' עברה (0.005 שניות).
```

לתרחישי בדיקה מתקדמים יותר, ייתכן ותבחר להתקין ספריות צד שלישי כמו Quick/Nimble, אשר מציעות תחביר יותר ביטויי לכתיבת בדיקות.

עם Quick/Nimble, ייתכן שתכתוב את אותה הבדיקה כך:

```swift
// הוסף את Quick ו-Nimble למנהל החבילות הזריז שלך או השתמש ב-CocoaPods/Carthage להתקנתם
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("מחשבון") {
            context("כאשר מחברים מספרים") {
                it("אמור להחזיר את הסכום הנכון") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

הרצת בדיקה זו תיתן לך פלט דומה בחלון הבדיקות או בלוג של כלי ה-CI/CD שלך, המציין אם הבדיקה הצליחה או נכשלה, עם פורמט יותר קריא לתיאור הבדיקות והציפיות.
