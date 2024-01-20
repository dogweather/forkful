---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वचन संयोजन (string concatenation) से अभिप्रेत है कि हम दो या दो से अधिक वचनों (strings) को जोड़ते हैं। प्रोग्रामर इसे करते हैं ताकि वे डाटा को कस्टम फॉर्मैट में प्रस्तुत कर सकें।

## कैसे:

वचन संयोजन के लिए सीपीपी उदाहरण⇩:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str1 = "नमस्ते ";
    std::string str2 = "दुनिया!";
    std::string finalStr = str1 + str2;

    std::cout << finalStr;  // "नमस्ते दुनिया!"
    return 0;
}
```
आउटपुट:

```C++
नमस्ते दुनिया!
```

## गहरी दुश्यांत:

1. ऐतिहासिक दृष्टिकोण: C भाषा के अग्रणी बनने से पहले, वचन संयोजन केवल अल्पविराम ऑपरेटर ('+'), जैसा कि FORTRAN में देखने को मिलता था, का इस्तेमाल करके किया जा सकता था।   
2. विकल्प: C++ ने वचनों के संयोजन के लिए अधिक प्रवद्ध और घटिया 'वचन केन्द्रित' पद्धतियों की पेशकश की है, जैसे कि '+=' ऑपरेटर।  
3. कार्यान्वयन विवरण: C++ में, वचन संयोजन जब हम '+' ऑपरेटर का इस्तेमाल करते हैं, तो वचन संयोजन वास्तव में एक नये अनुकरण का निर्माण करता है जिसमें दोनों वचनों की प्रतिलिपि होती है।  

## इसके अलावा देखें:

- सीपीपी रेफरेंस (वचन संयोजन):
https://en.cppreference.com/w/cpp/string/basic_string/operator%2B
- वचन संयोजन के उचित तरीकों का परिचय: 
https://stackoverflow.com/questions/18892281/most-idiomatic-way-to-concatenate-strings-in-c/18892333