---
title:                "Pustakon ka Lekhan"
html_title:           "Swift: Pustakon ka Lekhan"
simple_title:         "Pustakon ka Lekhan"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्ट कोडिंग ऐसे कामों में मदद करता है जो आप अपने स्वयं के कोड को सुनिश्चित करने के लिए या अपने कोड के साथ उपयोगित आसानियों को गुणवत्तापूर्ण बनाने के लिए कर रहे हों।

## कैसे करें

```Swift
// 1. टेस्ट सीडिंग के लिए XCTest लाइब्रेरी इंपोर्ट करें
import XCTest

// 2. टेस्ट केस स्थापित करें
class ExampleTests: XCTestCase {
    // 3. अपने टेस्ट केस के लिए एक नाम दें
    func testAddition() {
        // 4. प्रत्याशा बनाएं
        let a = 5
        let b = 10
        
        // 5. परिणाम की प्रत्याशा हमारे अपेक्षित परिणाम से मिलते हैं
        let expectedResult = 15
        
        // 6. टेस्ट के लिए कोड लिखें
        let result = a + b
        
        // 7. अपेक्षित परिणाम की टेस्ट करें
        XCTAssertEqual(result, expectedResult)
    }
}

// 8. टेस्ट को चलाएं और रिजल्ट देखें
// आपको दिखाई देना चाहिए "Test Passed"
```

## गहराई में

टेस्टिंग अपने स्वयं के कोड को सुनिश्चित करने के साथ-साथ एक अच्छा कोड क्वालिटी को भी आपके प्रोजेक्ट में जोड़ता है। सही टेस्ट केस को स्थापित करने और सही तरीके से टेस्ट कोड को लिखने के दौरान, आप अपने कोड को अधिक सुरक्षित बनाने में मदद मिल सकती है। साथ ही, टेस्ट कोड को बुनियादी बेसिक से शुरू करके आप इसे अपने कोड एक्सेस करने के लिए हमेशा उपयोग कर सकते हैं जो आपको अपने ऑब्जेक्ट्स के लिए एक लम्बे समय तक मौजूद रखने की जरूरत नहीं होती है।

## देखें भी

- [XCTest Documentation](https://developer.apple.com/documentation/xctest)