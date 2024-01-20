---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग की लंबाई का पता लगाना मतलब होता है कि स्ट्रिंग में कितने अक्षर हैं। यह उद्घोषण में कितना स्थान चाहिए, उपयोगकर्ता इनपुट सत्यापित करने, और डेटा संगठन करने के लिए बहुत महत्वपूर्ण है। 

## कैसे करें:

Lua में स्ट्रिंग की लंबाई जांचने के लिए, हम "#" ऑपरेटर का उपयोग करते हैं। 

```Lua
string1 = "नमस्ते"
print(#string1)
```
आउटपुट

```Lua
4
```
यहां "नमस्ते" स्ट्रिंग में 4 अक्षर हैं, इसलिए आउटपुट 4 है।

## गहराई में:

स्ट्रिंग की लंबाई जांचना Lua प्रोग्रामिंग भाषा का एक मूल घटक है और यह Lua के विभिन्न संस्करणों में विरासत में रहता है। इस काम को करने के लिए, `len` जैसी अन्य फ़ंक्शन का भी इस्तेमाल किया जा सकता है लेकिन "#" ऑपरेटर का उपयोग करना सबसे सीधा और आसान तरीका होता है। 

## अधिक जानकारी के लिए:

यदि आपको स्ट्रिंग लेंथ को और गहराई से समझने की आवश्यकता है, तो निम्नलिखित लिंक देखें: 

- [Lua String Documentation](http://lua-users.org/wiki/StringLibraryTutorial)
- [Lua String Length Operator](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Lua Programming/Strings](https://en.wikibooks.org/wiki/Lua_Programming/strings)