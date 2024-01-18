---
title:                "उप-शब्द निकालना"
html_title:           "Lua: उप-शब्द निकालना"
simple_title:         "उप-शब्द निकालना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अक्सर हमें पहले से दिए गए पूर्णस्थान (strings) के बीच से डेटा को निकालने की जरूरत होती है। यह हमारे द्वारा बनाए गए प्रोग्राम में कई महत्वपूर्ण कार्यों के लिए बहुत ही आवश्यक हो सकता है।

## कैसे करें?
```Lua
-- पूर्णस्थान (string) बनाएं
local str = "हेलो लुआ"

-- पूर्णस्थान (string) से उपस्थिति (position) निकालें
local pos = string.find(str, "लुआ")

-- उपस्थिति (position) के बाद का उपस्थान (substring) बनाएं
local substr = string.sub(str, pos+4)

print(substr) -- लुआ
```

## गहराई तक जाएं
पूर्णस्थान (string) से उपस्थिति (position) निकालने की प्रक्रिया हमारे प्रोग्राम को और भी और पोवरफुल बना सकती है। इससे हम अपनी प्रोग्राम को और भी ज्यादा एफीशिएंट बना सकते हैं। इसके अलावा, `string.sub()` के लिए अन्य विकल्प भी हैं जो हमें अनुकूलित कर सकते हैं, जैसे कि अलग-अलग भाषाओं में लिखे गए पूर्णस्थान (string) के बीच से उपस्थान (substring) निकालने के लिए `string.gmatch()` चुनाव भी हैं। अंतिम लक्ष्य हमेशा यह होना चाहिए कि हम अपनी प्रोग्रामिंग की योग्यताएं बढ़ाएं और सुनिश्चित करें कि हमारे प्रोग्राम दूरस्थ सेवाओं (remote services) तक भेजे गए डेटा को सही तरह से निकाल सकें।

## और भी देखें
- [विकिपीडिया पूर्णस्थान (substring)](https://hi.wikipedia.org/wiki/पूर्णस्थान_(समानार्थता))
- [Lua पूर्णस्थान (string) लाइब्रेरी](https://www.lua.org/pil/20.html)