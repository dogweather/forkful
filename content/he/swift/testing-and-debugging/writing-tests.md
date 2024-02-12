---
title:                "כתיבת בדיקות"
aliases:
- /he/swift/writing-tests/
date:                  2024-02-03T19:32:42.850753-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות בשפת Swift כוללת יצירת והרצת קוד שמבצע אימות של נכונות יחידות קוד אחרות באפליקציה שלך. מתכנתים עושים זאת כדי להבטיח אמינות, לזהות באגים מוקדם במחזור הפיתוח, ולהקל על שינויי קוד בעתיד ללא השלכות בלתי רצויות.

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
