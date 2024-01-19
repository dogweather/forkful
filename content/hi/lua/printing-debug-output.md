---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Debug output का मतलब है कोड के चलते हुए हमें जो डाटा मिलता है, उसे print करना। प्रोग्रामर्स इसे बग्स को ट्रैक करने, डाटा flows के विश्लेषण एवं वितर्तन की जांच करने के लिए करते हैं।

## कैसे करें:

Lua में debug output को print करने के लिए, आप `print` function का उपयोग कर सकते हैं।

```Lua
local x = 10
print("Debug: x = " .. x)
```

Output:

```Lua
Debug: x = 10
```

## गहराई में:

१. हिस्टरिकल कॉन्टेक्स्ट:
Lua में `print` function का इस्तेमाल करके debug output को प्रिंट करने का मुख्य तरीका हमेशा से रहा है।

२. विकल्प:
इसके अलावा `io.write` function का भी इस्तेमाल किया जा सकता है, जो मुद्रित output को format करने में अधिक flexibility देता है।

३. आधारभूत विवरण:
Lua में debug output को print करने के लिए, `print` या `io.write` function को strings के साथ concatenate किया जाता है।

## भी देखें:

Lua से संबंधित अधिक जानकारी के लिए निम्नलिखित स्रोतों का अन्वेषण करें:

1. Official Lua 5.4 reference manual: https://www.lua.org/manual/5.4/
2. Lua-Users wiki: http://lua-users.org/wiki/
3. Stack Overflow's Lua questions: https://stackoverflow.com/questions/tagged/lua