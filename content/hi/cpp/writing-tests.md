---
title:                "परीक्षाएं लिखना"
html_title:           "C++: परीक्षाएं लिखना"
simple_title:         "परीक्षाएं लिखना"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

परीक्षण तैयार करने में समय बिताने का यह श्रेष्ठ कारण है क्योंकि यह आपके कोड को खराब होने से बचा सकता है और आपको हमेशा एक सुरक्षित बारीकी के साथ नया कोड जोड़ने में मदद कर सकता है।

## कैसे करें

```C++
// Test code example
#include <iostream>
#include <cassert>

using namespace std;

int main(){
    // Test case 1
    assert(2+2 == 4); // This will pass
    
    // Test case 2
    int num = 5;
    assert(num < 3); // This will fail and throw an error message
    
    return 0;
}
```

Output:
```
Assertion failed!

Program terminated with exit code -1
```

उपरोक्त कोड उदाहरण आपको अपने कोड में कैसे परीक्षण जोड़ना है समझाता है। आप ```assert()``` फ़ंक्शन का उपयोग कर सकते हैं इसे गुणवत्ता के संलग्नता दर्ज करने के लिए। आप अपने टेस्ट का संख्या बढ़ा सकते हैं और नए स्थिति जाँच सकते हैं कि यह सही है या नहीं।

## गहराई में जाएँ

परीक्षण लिखना एक अच्छा सरंचना योजना और दृष्टिकोण विकसित करने का अवसर प्रदान कर सकता है। आप अपने कोड को कम रख सकते हैं और सोफ्टवेयर इंजीनियरिंग के मूल्यपूर्ण सिद्धांतों को समझने में मदद कर सकते हैं।

## देखें

[परीक्षण के लिए "तैयार" कैसे करें](https://simpleprogrammer.com/getting-started-writing-unit-tests/)

[परीक्षण लिखने के लिए 5 वेस्ट केस मिथक](https://www.martinfowler.com/articles/microservice-testing/#varying-test-content)

[वैध कारण: सूची](https://www.softwaretestinghelp.com/list-of-reasons-for-testing/)