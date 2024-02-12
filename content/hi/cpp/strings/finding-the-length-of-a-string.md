---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases:
- /hi/cpp/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:18.712915-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग की लंबाई जानना यह दिखाता है कि स्ट्रिंग में कितने कैरेक्टर हैं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डेटा को संसाधित कर सकें, मेमोरी उपयोग का पता लगा सकें, और सुरक्षा जैसे मानकों का पालन कर सकें।

## How to: (कैसे करें:)
```cpp
#include <iostream>
#include <string>

int main() {
    std::string myString = "नमस्ते दुनिया!";
    
    // लंबाई ढूँढना
    std::cout << "स्ट्रिंग की लंबाई: " << myString.length() << std::endl;
    
    return 0;
}
```
आउटपुट:
```
स्ट्रिंग की लंबाई: 14
```

## Deep Dive (विस्तार से जानकारी):
लंबाई ढूँढने के लिए C++ में `length()` फंक्शन का उपयोग करना एक मानक तरीका है, जो `std::string` क्लास का हिस्सा है। 

1. हिस्टोरिकल कॉन्टेक्स्ट: पहले C में स्ट्रिंग्स को `char` ऐरे के रूप में मैनेज किया जाता था, जिसमें `\0` (नल कैरेक्टर) का उपयोग समाप्ति के संकेत के रूप में होता था। स्ट्रिंग की लंबाई ढूँढने के लिए `strlen()` फंक्शन का उपयोग होता था, जो `string.h` हेडर में होता था।

2. विकल्प: `length()` के अलावा, `size()` फंक्शन भी `std::string` की लंबाई जानने के लिए उपयोग होता है, दोनों ही मेथड समान परिणाम देते हैं।

3. कार्यान्वयन विवरण: `std::string` ऑब्जेक्ट की लंबाई पहले से ही उसकी इंटरनल स्टेट में संग्रहीत होती है, इसलिए `length()` या `size()` को कॉल करना O(1) ऑपरेशन है - यानी यह हमेशा तेजी से एक समान समय में पूरा होता है, स्ट्रिंग की लंबाई से इसका संबंध नहीं होता।

## See Also (इसे भी देखें):
- C++ `std::string` पर और जानने के लिए [cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string).
- एक अन्य महत्वपूर्ण स्ट्रिंग ऑपरेशन `find()` की मदद से सबस्ट्रिंग कैसे ढूंढे, इस पर [cplusplus.com](http://www.cplusplus.com/reference/string/string/find/) पर विस्तार से पढ़ें।
- C++ नल कैरेक्टर के बारे में और समझने के लिए `std::string::npos` पर [cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/npos) पर जानकारी हासिल करें।
