---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# C++ में substring निकालने का तरिका 

## क्या और क्यों?

Substring एक string का ऐसा हिस्सा है, जिसे ताकि पूरी string के किसी विशेष भाग को अलग से उपयोग कर सकें, उसे अलग किया जाता है। इसे प्रोग्रामर डाटा को प्लास्टिसिटी देने, और कोड को पठनीय और मेंटेनेबल बनाने के लिए करते हैं।

## कैसे करें:

आप C++ की `substr()` फ़ंक्शन का उपयोग करके substring निकाल सकते हैं। 

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::string sub = str.substr(7, 5);
    std::cout << sub;
    return 0;
}
```
इस कार्यक्रम का आउटपुट `World` होगा, जो string का 7 वें स्थान से लेकर 5 अक्षरों का हिस्सा है।

## गहरी डाइव:

C++98 से hi `substr()` फ़ंक्शन C++ में मौजूद है। आप इस फ़ंक्शन का उपयोग करके किसी string से उसका कोई भी भाग निकाल सकते हैं। 

वैकल्पिक तरीके में, आप `std::string::find` और `std::string::find_first_of` का उपयोग भी कर सकते हैं। 

`substr()` के आंतरिक विस्तार के बारे में बात करें तो, यह फ़ंक्शन एक नई स्ट्रिंग ऑब्जेक्ट बनाता है और उसे रिटर्न करता है। इसलिए, इसका उपयोग स्मरण दृष्टि से बड़ी स्ट्रिंग्स के लिए सावधानी से किया जाना चाहिए। 

## अधिक जानकारी के लिए:

* C++ स्ट्रिंग के डॉक्युमेंटेशन: https://en.cppreference.com/w/cpp/string/basic_string
* सबस्ट्रिंग के लिए जीकल प्रोग्रामिंग की गाइड: https://www.geeksforgeeks.org/c-program-print-part-string/
* स्टैकओवरफ्लो पर स्ट्रिंग्स से संबंधित प्रश्न: https://stackoverflow.com/questions/tagged/c%2B%2B+string