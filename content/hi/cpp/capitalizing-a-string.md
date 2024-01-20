---
title:                "एक स्ट्रिंग को बड़े अक्षर में बदलना"
html_title:           "C++: एक स्ट्रिंग को बड़े अक्षर में बदलना"
simple_title:         "एक स्ट्रिंग को बड़े अक्षर में बदलना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? :
किसी स्ट्रिंग को कैपिटलाइज करना यानि उसमें जीवन्तता और स्पष्टता लाने के लिए उसके प्रथम अक्षरों को बड़े अक्षरों में बदलना। प्रोग्रामर इसे डाटा को सजीला और पढ़ने योग्य बनाने के लिए करते हैं।

## कैसे :
C++ में string के पहले अक्षर को uppercase में कैसे बदलें;
```C++
#include <iostream>
#include <cctype> //For using toupper function

int main() {
    std::string s = "hello world!";
    s[0] = std::toupper(s[0]);
    std::cout <<  s;
    return 0;
}
```
आउटपुट :
```C++
Hello world!
```
## गहराई में:
ऐतिहासिक दृष्टिकोण से, strings को capitalize करने की आवशयकता इसलिए होती है क्योंकि यह उसे पढ़ने में मदद करता है। विकल्पों में, Java जैसी अन्य प्रोग्रामिंग भाषाओं में string method का उपयोग करके string को capitalize करने का विकल्प होता है। C++ में string को capitalize करने के लिए, आपको अपने कोड में toupper function का उपयोग करना होता है, जो इस परिवर्तन को संभावित बनाता है।

## देखें भी :
1. [cplusplus.com: toupper](http://www.cplusplus.com/reference/cctype/toupper/)
2. [tutorialspoint: C++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm) 

आवश्यकता होने पर उपयोगी संसाधनों को खोजें, प्रयास करें, और जानें। सीखने का यही सबसे अच्छा तरीका है।