---
title:                "Swift: प्रोग्रामिंग में लिखना"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Kyu

Testing aapki Swift applications mein bahut zaroori hai kyuki ye aapko code ke bugs aur errors ko pehle se hi pata karne aur solve karne mein madad karta hai. Testing aapki application ki stability aur reliability ko bhi improve karta hai. 

## Kaise Karein

Testing ke liye aap XCTest framework ka istemaal kar sakte hain. Ismein aap apne code ko unit tests, UI tests aur performance tests ke liye alag-alag categories mein organize kar sakte hain. Neeche diye gaye code blocks mein aap dekh sakte hain ki kaise tests ko implement kiya jaata hai.

```Swift
import XCTest
@testable import MyApp

class MyTests: XCTestCase {

    // Unit Test Example
    func testMultiply() {
        let calculator = Calculator()
        XCTAssertEqual(calculator.multiply(2, 4), 8)
    }

    // UI Test Example
    func testLogin() {
        let app = XCUIApplication()
        app.launch()
        
        app.textFields["username"].tap()
        app.textFields["username"].typeText("John")
        
        app.secureTextFields["password"].tap()
        app.secureTextFields["password"].typeText("1234")
        
        app.buttons["login"].tap()
        
        XCTAssertTrue(app.staticTexts["Welcome John!"].exists)
    }

    // Performance Test Example
    func testPerformanceExample() {
        self.measure {
            let array = [1, 2, 3, 4, 5]
            for _ in 0...10000 {
                array.append(array.sum())
            }
        }
    }
}

```

## Deep Dive

Unit tests aapke code ke alag alag parts ko test karta hai jaise functions, extensions, etc. UI tests aapki application ke UI elements ko test karta hai jaise buttons, text fields, etc. Performance tests aapki application ki performance ko measure karta hai aur potential bottlenecks ko identify karne mein madad karta hai. Ek achi testing strategy aapki application ko reliable aur bug-free banane mein bahut madad karta hai.

## Dekhiye Bhi

- [Introduction to XCTest Framework](https://www.raywenderlich.com/960290-ui-testing-with-xctest-tutorial-getting-started)
- [Unit Testing in Swift](https://medium.com/flawless-app-stories/getting-started-with-unit-testing-in-swift-72ab0baea2d)
- [UI Testing in Swift](https://blog.xmartlabs.com/2016/07/07/xcode-ui-testing/)
- [Performance Testing in Swift](https://medium.com/expedia-group-tech/testing-performance-in-ios-62b81c27e622)