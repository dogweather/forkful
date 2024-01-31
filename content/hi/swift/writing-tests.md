---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेस्ट्स लिखने का मतलब है कोड के छोटे हिस्सों (यूनिट्स) की जाँच करना ताकि सुनिश्चित कर सकें कि वो सही ढंग से काम कर रहे हैं। प्रोग्रामर्स इसलिए टेस्ट्स लिखते हैं ताकि बग्स को पहले ही पकड़ सकें और बाद में कोड में बदलाव करते समय विश्वास के साथ कर सकें। 

## How to: (किस प्रकार:)
Swift में टेस्ट केस लिखने के लिए XCTest फ्रेमवर्क का इस्तेमाल किया जाता है। पहले हम एक साधारण फंक्शन बनाएंगे, फिर उसके लिए टेस्ट केस।

```Swift
// साधारण फंक्शन जो दो संख्याओं को जोड़ता है
func addNumbers(num1: Int, num2: Int) -> Int {
    return num1 + num2
}

// XCTest का इस्तेमाल करके टेस्ट केस लिखना
import XCTest

class MathTests: XCTestCase {
    func testAddNumbers() {
        XCTAssertEqual(addNumbers(num1: 2, num2: 3), 5)
    }
}

// XCTest का उपयोग करके टेस्ट्स को रन करना और परिणाम देखना
XCTMain([testCase(MathTests.allTests)])
```

जब आप उपरोक्त टेस्ट को रन करेंगे, तो कंसोल आउटपुट पर आपको सफल परीक्षण का मैसेज मिलेगा।

## Deep Dive (गहन अध्ययन)
टेस्टिंग की शुरुआत डेवलोपमेंट की प्रक्रिया के जितनी जल्दी हो सके उतनी जल्दी करना चाहिए (टेस्ट-ड्रिवेन डेवलपमेंट या TDD). XCTest के अलावा, कुछ अन्य फ्रेमवर्क जैसे Quick और Nimble भी हैं जो बीहेवियर-ड्रिवेन डेवलपमेंट (BDD) के लिए उपयोगी हैं। टेस्ट केस लिखने से ना केवल बग्स की पहचान होती है बल्कि कोड डिज़ाइन में भी सुधार होता है क्योंकि यह डेवलपर को रिफैक्टरिंग और मोड्यूलराइज़ेशन के लिए प्रोत्साहित करता है। 

## See Also (और भी देखें)
- एप्पल का XCTest टेस्टिंग फ्रेमवर्क: [XCTest Documentation](https://developer.apple.com/documentation/xctest)
- Swift के लिए पॉप्युलर BDD फ्रेम्वर्क Quick और Nimble: [Quick GitHub](https://github.com/Quick/Quick)
- टेस्ट-ड्राइवन डेवलपमेंट (TDD) के बारे में ज्यादा जानकारी: [TDD Tutorial](https://www.raywenderlich.com/709-ios-unit-testing-and-ui-testing-tutorial)
