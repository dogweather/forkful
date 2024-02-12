---
title:                "टेस्ट लिखना"
date:                  2024-02-03T19:33:09.676532-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Swift में टेस्ट लिखना ऐसे कोड को बनाने और चलाने की प्रक्रिया है जो आपके एप्लिकेशन में अन्य कोड यूनिट्स की सहीता की पुष्टि करता है। प्रोग्रामर इसे विश्वसनीयता सुनिश्चित करने, विकास चक्र में जल्दी बग का पता लगाने, और भविष्य में कोड रीफॅक्टरिंग को बिना अनइच्छित परिणामों के सुविधाजनक बनाने के लिए करते हैं।

## कैसे करें:
Swift, Xcode में एकीकृत XCTest फ्रेमवर्क के माध्यम से परीक्षण का समर्थन करता है। आप अपने कोड के व्यक्तिगत भागों की जांच के लिए यूनिट टेस्ट लिख सकते हैं, उदाहरण के लिए, दो संख्याओं का योग करने वाले एक फंक्शन का।

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "योग फंक्शन ने अपेक्षित मूल्य वापस नहीं किया।")
    }
}
```

इस टेस्ट को चलाने के लिए, आप आमतौर पर Xcode में Command-U दबाएँगे। Xcode टेस्ट नेविगेटर में आउटपुट आपको बताएगा कि टेस्ट पास हुआ या फेल।

उदाहरण के लिए, एक सफल टेस्ट आउटपुट:
```
टेस्ट केस '-[YourAppTests testSum]' पास हुआ (0.005 सेकंड)।
```

अधिक उन्नत परीक्षण परिदृश्यों के लिए, आप तीसरे पक्ष की लाइब्रेरीज जैसे कि Quick/Nimble का उपयोग कर सकते हैं, जो टेस्ट लिखने के लिए एक अधिक व्यक्तिगत सिंटैक्स प्रदान करती हैं।

Quick/Nimble के साथ, आप इसी तरह का टेस्ट ऐसे लिख सकते हैं:

```swift
// अपने Swift पैकेज मैनेजर में Quick और Nimble जोड़ें या उन्हें इंस्टॉल करने के लिए CocoaPods/Carthage का उपयोग करें
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Calculator") {
            context("when summing numbers") {
                it("should return the correct sum") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

इस टेस्ट को चलाने पर आपके टेस्ट कंसोल या CI/CD टूल के लॉग में समान आउटपुट मिलेगा, जो इंगित करेगा कि टेस्ट सफल रहा या असफल, टेस्ट और अपेक्षाओं का विवरण देने के लिए एक अधिक पठनीय प्रारूप के साथ।
