---
title:                "C++: स्ट्रिंग को निचे केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को निचे केस में रूपांतरित करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

हेलो दोस्तों! आज हम बात करेंगे C++ प्रोग्रामिंग के एक बहुत ही उपयोगी विषय के बारे में - जो हैं एक स्ट्रिंग को लोअर केस में बदलना। अक्सर हमारे कोड में स्ट्रिंग इंपुट को लोअर केस में लाने की जरूरत पड़ती है। इससे हम अलग-अलग स्ट्रिंग्स को सरलता से तुलनात्मक कर सकते हैं। आइए जानते हैं कि स्ट्रिंग को लोअर केस में बदलने का कोड कैसे लिखा जाता है।

## क्यों

स्ट्रिंग को लोअर केस में बदलने का मुख्य कारण है उस स्ट्रिंग को सरलता से तुलनात्मक बनाना। स्ट्रिंग को समान केस में रखने से हम उसे आसानी से तुलना कर सकते हैं जैसे कि मैं बात करता हूं की क्यों SVG और png फाइल होती है।

## कैसे

आप सरलता से [स्ट्रिंग](https://www.geeksforgeeks.org/converting-uppercase-lower-case-string/#string-functions-in-cpp) को लोअर केस में बदल सकते हैं, C++ में यह तरीका जानते हैं।

```C++
//आपने यह स्क्रिप्ट jockey.cpp नाम से सेव कर लिया।
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;
 
int main()
{
    string name = "HINDI BLOG POST";
    cout << "पहले नाम:" << name << endl;
 
    //प्रत्येक चर लोअर केस में बदलने के लिए इस्तेमाल किया गया है 
    transform(name.begin(), name.end(), name.begin(), ::tolower);
    cout << "नया नाम:" << name << endl;
 
    return 0;
}

```
आउटपुट:
```
पहले नाम: HINDI BLOG POST
नया नाम: hindi blog post
```

## घुमाने के लिए

अब हमें एक नज़र डीप डाइव में उस कोड के ऊपर डालने की जरूरत है जो मूरनिंग च्यूटोरीयल आउटपुट [.Flow](https://stackoverflow.com/questions/735204/convert-a-string-in