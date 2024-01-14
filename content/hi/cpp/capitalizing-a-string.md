---
title:                "C++: स्ट्रिंग प्रथमाकरण"
simple_title:         "स्ट्रिंग प्रथमाकरण"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

शब्दों को बड़ी अक्षरों में लिखने का काम बहुत अहम है। यह उपयोगकर्ता को दिखाता है कि कौन से शब्द प्रत्येक वाक्य के शुरुआत में हैं। इसके अलावा, यह प्रोग्राम को कम्प्यूटर को पढ़ने और समझने में आसानी प्रदान करता है।

## कैसे करें

```C++ 
#include<iostream> 
#include<string> 
using namespace std; 

int main() { 
    // स्ट्रिंग दर्ज करें 
    string str = "programming"; 
    
    // शब्दों को बड़ी लेटरों में बदलें 
    for(int i = 0; i < str.length(); i++) { 
        str[i] = toupper(str[i]); 
    } 
    
    // परिणाम प्रिंट करें 
    cout<<"परिणाम: "<<str; 
    
    return 0; 
} 
```

### परिणाम
```
परिणाम: PROGRAMMING
```

## गहराई तक खुद से खोज करें

जब हम ऊपर दिए गए कोड को गहराई से बात करते हैं, तो हम देख सकते हैं कि हम शब्दों को बड़ी अक्षरों में बदलने के लिए `toupper()` फ़ंक्शन का उपयोग करते हैं। इससे हमें स्ट्रिंग के प्रत्येक अक्षर पर गहराई से पहुंचने की और उसे बड़े फेस्टर मिलाने की अनुमति मिलती है। इससे यह बेहतर तरीके से चलेगा जब हम बड़ी स्ट्रिंग्स का उपयोग करते हैं।

## देखें भी

- [C++ String Functions](https://www.programiz.com/cpp-programming/library-function/cstring/toupper)
- [How to Capitalize the First Letter of a String in C++](https://www.techiedelight.com/capitalize-first-letter-of-string-cpp/)