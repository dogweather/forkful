---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:00.765342-07:00
description: "\u0915\u0948\u0938\u0947: C++ \u092E\u0947\u0902, \u0906\u092A \u0915\
  \u093F\u0938\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B\
  \ \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\
  \u094D\u0937 \u0915\u0947 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\
  \ \u091C\u094B\u095C\u0947 \u092C\u093F\u0928\u093E \u0938\u094D\u091F\u0948\u0902\
  \u0921\u0930\u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\u0964 \u0939\
  \u093E\u0932\u093E\u0902\u0915\u093F, \u0905\u0927\u093F\u0915 \u091C\u091F\u093F\
  \u0932 \u092F\u093E\u2026"
lastmod: '2024-04-05T21:53:54.783582-06:00'
model: gpt-4-0125-preview
summary: "C++ \u092E\u0947\u0902, \u0906\u092A \u0915\u093F\u0938\u0940 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\
  \u0932\u093E\u0907\u091C\u093C \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u0915\u0947 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u091C\u094B\u095C\u0947 \u092C\
  \u093F\u0928\u093E \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\
  \u093F, \u0905\u0927\u093F\u0915 \u091C\u091F\u093F\u0932 \u092F\u093E \u0935\u093F\
  \u0936\u093F\u0937\u094D\u091F \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\
  \u0947\u0936\u0928 \u0935\u094D\u092F\u0935\u0939\u093E\u0930\u094B\u0902 \u0915\
  \u0947 \u0932\u093F\u090F, \u091C\u0948\u0938\u0947 \u0915\u093F Boost \u092C\u0939\
  \u0941\u0924 \u0938\u0939\u093E\u092F\u0915 \u0939\u094B \u0938\u0915\u0924\u0940\
  \ \u0939\u0948\u0964 \u0928\u0940\u091A\u0947 \u0926\u094B\u0928\u094B\u0902 \u0926\
  \u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923\u094B\u0902 \u0915\u094B \u0926\
  \u0930\u094D\u0936\u093E\u0928\u0947 \u0935\u093E\u0932\u0947 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

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
