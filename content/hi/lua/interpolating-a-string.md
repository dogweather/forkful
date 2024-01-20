---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग इंटरपोलेशन Lua प्रोग्रामिंग में एक तरीका है जिसमें हम कोड में स्ट्रिंग की कई भागों को जोड़ सकते हैं। प्रोग्रामर इसे उपयोग करते हैं क्योंकि यह कई विभिन्न मूल्यों को एक साथ जोड़ने में मदद करता है, बिना कोड को जटिल बनाए। 

## कैसे करें:
लेट्स सी एक साधारण कोड का उदाहरण:

```Lua
-- string interpolation
name = "Lua"
print(("Hello %s World!"):format(name))
```
इसका आउटपुट होगा :

```Lua
Hello Lua World!
```
## गहराई में:
स्ट्रिंग इंटरपोलेशन का समर्थन Lua के प्रारंभिक संस्करणों से ही रहा है। हालांकि, इसे सीधे स्ट्रिंग लिट्रल के रूप में उपयोग करने का एक वैकल्पिक तरीका भी है, जिसे 'टेम्पलेट स्ट्रिंग' कहा जाता है। इसके लिए, `string.format` फ़ंक्शन का उपयोग करना होगा। 

ये स्ट्रिंग्स C भाषा के स्प्रिंटफ फ़ंक्शन के समान होते हैं, जो एक आम तरीका है जिसका उपयोग प्रोग्रामर्स स्ट्रिंग इंटरपोलेशन के लिए करते हैं। 

## भी देखें:
1. [Lua स्ट्रिंग इंटरपोलेशन](https://www.lua.org/pil/20.4.html)
2. [Lua स्ट्रिंग लिब्रेरी](https://www.lua.org/manual/5.1/manual.html#5.4)
3. [सी स्प्रिंटफ फ़ंक्शन](https://www.cplusplus.com/reference/cstdio/sprintf/)