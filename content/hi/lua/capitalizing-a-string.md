---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Lua: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?

एक string को महत्त्वपूर्ण बनाना मतलब उसके पहले अक्षर को बड़ा करना है। प्रोग्रामर इसे यह सुनिश्चित करने के लिए करते हैं कि उनके प्रोग्राम में किसी भी string का पहला अक्षर बड़ा हो, जो एक नियम, कन्वेंशन, या उपयोगकर्ता इंटरफ़ेस संकेत प्रदान कर सकता है।

## कैसे:

```Lua
s = "lua scripting language"
s = s:sub(1,1):upper()..s:sub(2)
print(s)
```
आउटपुट:
```Lua
Lua scripting language
```

## गहरा डाइव

(1) Lua का "string.sub" और "string.upper" विधियाँ प्राचीन समय से ही string में किसी भी विशेष अक्षर को बड़ा करने के लिए उपयोग में ली जाती हैं। 

(2) विकल्प संभव हैं, लेकिन Lua में, नीचे दिए गए कोड स्निपेट का उपयोग करके string के पहले अक्षर को कैपिटलाइज करना सबसे अधिक प्रभावी और सीधा है। 

(3) इस विधि का समर्थन करने के लिए, Lua अंतरिक रूप से Unicode को नहीं समर्थन करता है, इसलिए अतिरिक्त बाहरी पुस्तकालयों की आवश्यकता हो सकती है जब आप non-ASCII characters के साथ काम कर रहे हों। 

## देखें भी

1. Lua string library: http://www.lua.org/manual/5.3/manual.html#6.4
2. Lua tutorial on string manipulation: https://www.tutorialspoint.com/lua/lua_strings.htm
3. Lua User wiki on strings: http://lua-users.org/wiki/StringLibraryTutorial