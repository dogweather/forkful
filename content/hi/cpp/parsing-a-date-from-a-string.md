---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक डेट को स्ट्रिंग से पार्स करने का मतलब है एक इनपुट स्ट्रिंग को एक डेट ऑब्जेक्ट में बदलना। प्रोग्रामर इसे तारीख को इनपुट के रूप में लेने और उसे सही फॉर्मेट में परिवर्तित करने के लिए करते हैं।

## कैसे करें:

निम्नलिखित कोड ब्लॉक C++ में डेट पार्सिंग का उदाहरण है:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    const std::string s = "2021-09-15";

    std::istringstream ss(s);
    std::tm t = {};
    ss >> std::get_time(&t, "%Y-%m-%d");

    if (ss.fail()){
        std::cout << "Parse failed\n";
    } else {
        std::cout << std::put_time(&t, "%c") << '\n';
    }
  
    return 0;
}
```
यहाँ `"2021-09-15"` को डेट के रूप में परिवर्तित किया जा रहा है। यदि पार्स सफल होता है, तो यह डेट मानक प्रारूप में प्रिंट करेगा, अन्यथा "Parse failed" मैसेज प्रिंट करेगा। 

## गहराई में: 

### ऐतिहासिक प्रसंग:
तारीख पार्सिंग एक आम क्रिया है जिसे कंप्यूटर के शुरुआती दिनों से ही किया जा रहा है। यह डेटा विश्लेषण, प्रदर्शन, और संगठन में बहुत महत्वपूर्ण होता है।

### विकल्प:
C++ में `std::get_time` और `std::put_time` के अलावा, `std::strftime` और `std::strptime` फ़ंक्शंस भी डेट पार्सिंग के लिए उपलब्ध हैं। 

### क्रियान्वयन विवरण:
C++ में `std::get_time` और `std::put_time` फ़ंक्शंस POSIX समय तारीख प्रारूप (जैसे `%y,%m,%d`) का समर्थन करते हैं।

## अन्य देखें:

- [C++ डेट और टाइम (cplusplus.com)](http://www.cplusplus.com/reference/ctime/)
- [C++ पार्सिंग स्ट्रिंग्स (cplusplus.com)](http://www.cplusplus.com/reference/sstream/istringstream/istringstream/)