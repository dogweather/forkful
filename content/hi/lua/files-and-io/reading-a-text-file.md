---
date: 2024-01-20 17:55:27.142041-07:00
description: "\u092A\u093E\u0920\u094D\u092F \u092B\u093C\u093E\u0907\u0932 \u092A\
  \u0922\u093C\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u092E\
  \u0947\u0902 \u0926\u093F\u090F \u0917\u090F \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u092A\u0922\u093C\u0928\u093E \u0914\u0930 \u0909\u0938\u0947 \u0938\u092E\u091D\
  \u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0906\u092E \u0924\u094C\u0930 \u092A\u0930 \u092F\u0942\
  \u091C\u0930 \u0921\u0947\u091F\u093E, \u0915\u0949\u0928\u094D\u092B\u093F\u0917\
  \u0930\u0947\u0936\u0928\u094D\u0938, \u092F\u093E \u0932\u0949\u0917\u094D\u0938\
  \ \u0915\u094B \u092A\u094D\u0930\u094B\u0938\u0947\u0938 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:52.579928-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920\u094D\u092F \u092B\u093C\u093E\u0907\u0932 \u092A\u0922\
  \u093C\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u092E\u0947\
  \u0902 \u0926\u093F\u090F \u0917\u090F \u0921\u0947\u091F\u093E \u0915\u094B \u092A\
  \u0922\u093C\u0928\u093E \u0914\u0930 \u0909\u0938\u0947 \u0938\u092E\u091D\u0928\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0906\u092E \u0924\u094C\u0930 \u092A\u0930 \u092F\u0942\u091C\
  \u0930 \u0921\u0947\u091F\u093E, \u0915\u0949\u0928\u094D\u092B\u093F\u0917\u0930\
  \u0947\u0936\u0928\u094D\u0938, \u092F\u093E \u0932\u0949\u0917\u094D\u0938 \u0915\
  \u094B \u092A\u094D\u0930\u094B\u0938\u0947\u0938 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ्य फ़ाइल पढ़ने का मतलब उसमें दिए गए डेटा को पढ़ना और उसे समझना है। प्रोग्रामर्स आम तौर पर यूजर डेटा, कॉन्फिगरेशन्स, या लॉग्स को प्रोसेस करने के लिए पाठ्य फ़ाइल पढ़ते हैं।

## How to: (कैसे करें:)
चलिए Lua में एक साधारण पाठ्य फाइल कैसे पढ़ी जाती है, ये देखें:

```Lua
-- फ़ाइल खोलना
local file = io.open("example.txt", "r")

-- फ़ाइल की सामग्री पढ़ना
if file then
    local content = file:read("*a")  -- पूरी फाइल पढ़ें
    print(content)                   -- सामग्री प्रिंट करें
    file:close()                     -- फ़ाइल बंद करें
else
    print("नहीं खुल सकती फ़ाइल।")
end
```

अगर `example.txt` में "Hello, Lua!" लिखा है, तो आउटपुट होगा:
```
Hello, Lua!
```

## Deep Dive (गहराई से जानिए)
पुराने ज़माने में, डेटा बड़े-बड़े कम्प्यूटर टेप्स पर सेव होता था। वक्त के साथ, सिम्पल टेक्स्ट फ़ाइलें कॉमन हो गईं, क्योंकि वे आसान थीं पढ़ने और लिखने के लिए। Lua में `io` लाइब्रेरी फ़ाइल I/O (input/output) ऑपरेशंस के लिए है। `io.open()` फ़ाइल खोलने के लिए है; इसके बाद `read()` से हम सामग्री पढ़ सकते हैं। "*a" पूरी फ़ाइल पढ़ने का पैरामीटर है।

वैकल्पिक रूप में, `io.lines()` से एक-एक करके लाइन पढ़ सकते हैं, या नेटवर्क स्ट्रीम से डेटा पढ़ने के लिए `socket` लाइब्रेरीज़ का उपयोग कर सकते हैं।

फ़ाइल पढ़ते समय याद रखें, फ़ाइल पढ़ने की प्रक्रिया धीमी हो सकती है, खासकर बड़ी फ़ाइल्स के लिए। कोड लिखते समय इसका ध्यान रखें।

## See Also (और देखिए)
1. Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
2. Programming in Lua (first edition): http://www.lua.org/pil/1.html
3. Lua-Users Wiki: http://lua-users.org/wiki/IoLibraryTutorial

नोट: Lua के ऑनलाइन संसाधन अंग्रेजी में हो सकते हैं, लेकिन वे Lua को सीखने और माहिर बनने के लिए महत्वपूर्ण हैं।
