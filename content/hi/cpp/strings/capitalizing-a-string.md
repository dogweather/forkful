---
title:                "स्ट्रिंग को कैपिटलाइज करना"
date:                  2024-02-03T19:06:00.765342-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी स्ट्रिंग को कैपिटलाइज़ करना इसके प्रत्येक शब्द के प्रारम्भिक अक्षर को यदि वह लोअरकेस में है तो अपरकेस में परिवर्तित करने की प्रक्रिया है, जबकि शेष अक्षरों को अपरिवर्तित रखते हुए। प्रोग्रामर आमतौर पर आउटपुट्स, यूजर इनपुट्स, या डाटा प्रोसेसिंग को फॉर्मेट करने के लिए यह कार्य करते हैं ताकि टेक्स्ट को पेश या प्रोसेस किए जाने में सुसंगति सुनिश्चित की जा सके, विशेष रूप से यूजर इंटरफेसेस या डेटा नॉर्मलाइजेशन कार्यों में।

## कैसे:
C++ में, आप किसी स्ट्रिंग को कैपिटलाइज़ कर सकते हैं तृतीय-पक्ष के लाइब्रेरी जोड़े बिना स्टैंडर्ड लाइब्रेरी का उपयोग करके। हालांकि, अधिक जटिल या विशिष्ट कैपिटलाइजेशन व्यवहारों के लिए, जैसे कि Boost बहुत सहायक हो सकती है। नीचे दोनों दृष्टिकोणों को दर्शाने वाले उदाहरण दिए गए हैं।

### स्टैंडर्ड C++ लाइब्रेरी का उपयोग:

```cpp
#include <iostream>
#include <cctype> // std::tolower और std::toupper के लिए
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // आउटपुट: "Hello World From C++"
}
```

### Boost लाइब्रेरी का उपयोग:

अधिक उन्नत स्ट्रिंग मैनिपुलेशन के लिए, जैसे कि स्थानीय-जागरूक कैपिटलाइजेशन, आप Boost String Algo लाइब्रेरी का उपयोग करना चाह सकते हैं।

पहले, सुनिश्चित करें कि आपके पास Boost लाइब्रेरी आपके प्रोजेक्ट में स्थापित और कॉन्फिगर की गई है। तब आप आवश्यक हेडर्स को शामिल कर सकते हैं और नीचे दिखाए गए अनुसार इसकी सुविधाओं का उपयोग कर सकते हैं।

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज़ करें
    boost::algorithm::to_lower(capitalizedText); // सुनिश्चित करें कि स्ट्रिंग लोअरकेस में है
    capitalizedText[0] = std::toupper(capitalizedText[0]); // पहले अक्षर को कैपिटलाइज़ करें

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // एक स्पेस के बाद कैपिटलाइज़ करें
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // आउटपुट: "Hello World From C++"
}
```

इस मामले में, Boost कुछ स्ट्रिंग मैनिपुलेशन कार्यों को सरल बनाता है लेकिन सच्चे कैपिटलाइजेशन के लिए एक अनुकूलित दृष्टिकोण की आवश्यकता होती है क्योंकि यह मुख्य रूप से परिवर्तन और केस परिवर्तन उपयोगिताओं की पेशकश करता है।
