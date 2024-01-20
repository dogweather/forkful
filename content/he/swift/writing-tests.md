---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך של יצירת מבחנים אוטומטיים לקוד שתוכנן לבדוק את התנהגותו כנדרש. תוכניתנים עושים זאת כדי לוודא שהתוכנה עובדת נכון, למזער באגים ולקל על תהליכי שינוי ושדרוג בעתיד.

## איך לעשות:
```Swift
import XCTest

class MyTests: XCTestCase {

    func testExample() {
        // זהו הקוד שאת/ה בודק/ת
        let result = "Hello, World!"
        // זו הטענה של הבדיקה -> מה צריך להיות התוצאה
        XCTAssertEqual(result, "Hello, World!", "התוצאה צריכה להיות Hello, World!")
    }

    func testAnotherThing() {
        // בדיקה נוספת
        let value = 5
        XCTAssertTrue(value > 0, "הערך צריך להיות גדול מ-0")
    }
}

MyTests.defaultTestSuite.run()
```

פלט לדוגמה:
```
Test Suite 'MyTests' started at 2023-01-01 00:00:00.000
Test Case '-[MyTests testExample]' passed (0.001 seconds).
Test Case '-[MyTests testAnotherThing]' passed (0.001 seconds).
Test Suite 'MyTests' finished at 2023-01-01 00:00:00.002.
```

## צלילה לעומק
כתיבת בדיקות אוטומטיות החלה בשנים הראשונות של תוכנה, אבל רק בשנים האחרונות הפכה לחלק מרכזי בפיתוח תוכנה מודרני, באמצעות שיטות כמו TDD (Test-Driven Development). אלטרנטיבות ל-XCTest כוללות מסגרות כמו Quick/Nimble. פרטים טכניים כוללים הבנה עמוקה יותר של Assertions וmocking לסימולציה של פעולות ואינטראקציות.

## ראה גם
- [מדריך ל-XCTest של Apple](https://developer.apple.com/documentation/xctest)
- [TDD למתחילים](https://www.raywenderlich.com/5522-test-driven-development-tutorial-for-ios-getting-started)
- [Quick – מסגרת בדיקות BDD](https://github.com/Quick/Quick)