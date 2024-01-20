---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रेगुलर एक्सप्रेशन (pattern) से मेल खाने वाले वर्णों को हटाना, प्रोग्रामिंग में एक सामान्य कार्य है। इसे इसलिए किया जाता है ताकि हम उन वर्णों को दूर कर सकें जिनकी हमें आवश्यकता नहीं होती, और हमारे डाटा को साफ और त्रुटिमुक्त बनाएँ। 

## कैसे:

Lua में आप ```string.gsub()``` फ़ंक्शन का उपयोग करके, एक पैटर्न से मिलने वाले वर्णों को हटा सकते हैं।

```Lua
s = "Hello, Wॉrld!"
s = s:gsub("[,ॉ]","") -- कॉमा और विशेष वर्ण "ॉ" को हटाने के लिए
print(s)   -- Output: Hello Wrld!
```

## गहराई में

Lua में रेगुलर एक्सप्रेशन के pattern से मिलने वाले वर्णों को हटाना दीक्षित और औलचीन ढंग से डाटा को संशोधित करने का एक महत्वपूर्ण तरीका है। 

Lua भाषा में यह क्षमता 1.0 संस्करण से ही मौजूद थी। वैकल्पिक रूप से, आप Lua के ```string.find()``` और ```string.sub()``` फ़ंक्शन्स का उपयोग करके भी इसे कर सकते हैं, लेकिन ```gsub()``` का उपयोग करना आमतौर पर अधिक कुशल और सीधा है।

## देखें भी

विषय से संबंधित अन्य स्रोतों के लिए, निम्नलिखित लिंक पर क्लिक करें।

[Regex tutorial — A quick cheatsheet by examples](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)

[String Manipulation in Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)

[Understanding Lua's gsub function](https://www.lua.org/pil/20.2.html)