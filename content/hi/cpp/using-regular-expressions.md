---
title:                "C++: रेगुलर एक्सप्रेशन का उपयोग करना"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

रेगुलर एक्सप्रेशन्स (`regex`) प्रोग्रामिंग में बहुत सारे उपयोग हो सकते हैं, जैसे कि स्ट्रिंग मैचिंग, पाठ प्रसंस्करण, और डेटा वैधता जांच आदि। इसलिए यह सिर्फ बहुत उपयोगी ही नहीं है, बल्कि प्रोग्रामिंग को सरल और दक्ष बनाने में भी मददगार सिद्ध हुआ है।

## कैसे करें

जब आप `regex` का उपयोग करते हैं, आप `#include <regex>` लाइब्रेरी को अपने कोड में जोड़ना होगा। सबसे पहले, आपको `regex` ऑब्जेक्ट बनाना होगा जिसमें आप अपने पैटर्न को कंपाइल कर सकेंगे। फिर, आप `smatch` (स्ट्रिंग मैचिंग) ऑब्जेक्ट बनाकर इसे पैस करेंगे। इसके बाद, आप `regex_search` फ़ंक्शन को यूज़ करके अपने स्ट्रिंग को अपने पैटर्न से जोड़ कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```C++
#include <iostream>
#include <regex>

using namespace std;

int main()
{
  string str = "यह एक simple स्ट्रिंग है।";
  regex pattern ("स्ट्रिंग");

  smatch matches;
  regex_search(str, matches, pattern);

  cout << "स्ट्रिंग मिल गया: " << matches[0] << endl;
  
  return 0;
}
```

उपरोक्त कोड का आउटपुट निम्न होगा:

```
स्ट्रिंग मिल गया: स्ट्रिंग
```

## गहराई में जाइये

`regex` प्रोग्रामिंग में कई मजेदार और उपयोगी फ़ंक्शन हैं, जैसे `regex_replace`, `regex_match`, और `regex_iterator`। आप अपने और अधिक प्रश्नों के लिए [यहां](https://www.cplusplus.com/reference/regex/) जांच सकते हैं। आपको यह भी ध्यान देना चाहिए कि `regex` पैटर्न C++11 से ही समर्थित हैं, इ