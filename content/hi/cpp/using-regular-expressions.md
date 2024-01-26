---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रेगुलर एक्सप्रेशंस (Regular Expressions) पैटर्न मैचिंग की एक तकनीक है, जिसे टेक्सट सर्च और मैनिपुलेशन के लिए इस्तेमाल किया जाता है। प्रोग्रामर इस उपयोगी उपकरण का इस्तेमाल जटिल पैटर्न खोजने और डेटा वैलिडेशन में करते हैं।

## कैसे करें:
C++ में रेगुलर एक्सप्रेशंस का इस्तेमाल करने के लिए कोड का उदाहरण और सैंपल आउटपुट:

```C++
#include <iostream>
#include <regex>
#include <string>
using namespace std;

int main() {
    // एक रेगुलर एक्सप्रेशन पैटर्न क्रिएट करें
    regex email_pattern("[\\w\\.-]+@\\w+\\.\\w+");

    // जांच करें कि दी गई स्ट्रिंग पैटर्न से मैच करती है या नहीं
    string email = "koi_email@example.com";
    if (regex_match(email, email_pattern)) {
        cout << "वैध ईमेल पता!" << endl;
    } else {
        cout << "अवैध ईमेल पता!" << endl;
    }
    return 0;
}
```

सैंपल आउटपुट:
```
वैध ईमेल पता!
```

## गहराई में जानकारी:
रेगुलर एक्सप्रेशंस का इतिहास 1950 के दशक में हुई थ्योरेटिकल वर्क से शुरू होता है। Perl भाषा रेगुलर एक्सप्रेशंस के मुख्य प्रसारकों में से एक थी। C++ में, `<regex>` हेडर फ़ाइल इसे इंप्लीमेंट करती है। रेगुलर एक्सप्रेशंस के विकल्प में टेक्स्ट पार्सिंग लाइब्रेरीज और स्ट्रिंग सर्च एल्गोरिदम हो सकते हैं, किन्तु इनकी लचीलापन और शक्ति उतनी नहीं होती जितनी रेगुलर एक्सप्रेशंस की होती है।

## इसे भी देखें:
- [Cplusplus.com - Regular Expressions Library](https://www.cplusplus.com/reference/regex/)
- [cppreference.com - std::regex](https://en.cppreference.com/w/cpp/regex)

इन लिंक्स पर जाकर आप और जानकारी पा सकते हैं और अपनी समझ बढ़ा सकते हैं।
