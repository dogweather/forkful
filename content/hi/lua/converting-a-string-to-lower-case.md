---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग को लोअरकेस में बदलना, इसे छोटे अक्षरों में परिवर्तित करना होता है। कोडर्स इसे तब करते हैं जब वे उपयोगकर्ता से इनपुट लेते हैं और उन्हें सुनिश्चित करना होता है कि केस सेंसिटिविटी से बाहर जाए। 

## कैसे करें:

Lua में, आप स्ट्रिंग:lower() फ़ंक्शन का उपयोग करके एक स्ट्रिंग को लोअर केस में बदल सकते हैं:

```Lua
str = "Hello, World!"
print(str:lower())
```

उपरोक्त कोड का आउटपुट होगा:

```Lua
"hello, world!"
```

## गहराई में:

* **ऐतिहासिक संदर्भ:** Lua में `string.lower` विधि विशेष रूप से यह सुनिश्चित करने के लिए बनाई गई थी कि कोडर्स को केस सेंसिटिविटी से बाहर जाने में मदद मिले।
* **विकल्प:** अगर आपको केवल पहला अक्षर छोटा करने की आवश्यकता है, तो आप `string.sub` और `string.char` का उपयोग कर सकते हैं।
* **कार्यान्वयन विवरण:** `string.lower` विधि Lua में प्रत्येक अक्षर को UTF-8 कोड पॉइंट मूल्य के रूप में लेती है और उसे अपने निम्न केस समकक्ष में बदलती है।

## भी देखें:

* [Lua मैनुअल `string.lower`](https://www.lua.org/manual/5.4/manual.html#6.4.2)
* [Lua String लाइब्ररी](https://www.lua.org/pil/20.html)
* [UTF-8](https://en.wikipedia.org/wiki/UTF-8)
* [Case Sensitivity in Programming (कोडिंग में केस सेंसिटिविटी)](https://www.computerhope.com/jargon/c/casesens.htm)