---
title:                "जटिल संख्याओं के साथ काम करना"
aliases:
- /hi/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:46:27.765282-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याओं में एक वास्तविक भाग और एक काल्पनिक भाग होता है (जैसे 3 + 4i)। प्रोग्रामर उनका उपयोग स्विफ्ट में सिग्नल प्रोसेसिंग, कुछ गणित समस्याओं को हल करने, और भौतिकी का सिमुलेशन करने के लिए करते हैं।

## कैसे:
स्विफ्ट में जटिल संख्या समर्थन नहीं है, लेकिन हम अपना खुद का बना सकते हैं:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // अतिरिक्त तरीके जैसे घटाना, गुणा करना, आदि
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("परिणाम: \(result.real) + \(result.imaginary)i")
// नमूना उत्पादन: परिणाम: 3.0 + 7.0i
```

## गहराई से परिचय
जटिल संख्याएँ 16वीं शताब्दी में बीजगणितीय समीकरणों में उत्पन्न हुईं। वे क्वांटम यांत्रिकी, नियंत्रण सिद्धांत, और अन्य कई क्षेत्रों में अत्यावश्यक हैं। एप्पल के स्विफ्ट में पाइथन या C++ जैसी भाषाओं के विपरीत जटिल संख्याओं के लिए एक मानक पुस्तकालय नहीं है। अपना खुद का बनाने के विकल्पों में न्यूमेरिक्स पैकेज का उपयोग शामिल है जिसमें जटिल संख्याओं का समर्थन शामिल है या स्विफ्ट की अन्तर क्रियाशीलता के साथ C++ जटिल पुस्तकालय को लपेटना शामिल है।

## देखें भी
- स्विफ्ट न्यूमेरिक्स: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
