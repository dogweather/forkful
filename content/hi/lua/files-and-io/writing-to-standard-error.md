---
title:                "मानक त्रुटि के लिए लिखना"
aliases:
- hi/lua/writing-to-standard-error.md
date:                  2024-02-03T19:34:26.814324-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
मानक त्रुटि (stderr) में लिखना त्रुटि संदेशों और नैदानिक आउटपुटों को मानक आउटपुट (stdout) से अलग, एक पृथक चैनल में निर्देशित करने के बारे में है। प्रोग्रामर इसे त्रुटि जानकारी से नियमित प्रोग्राम परिणामों को अलग करने, डिबगिंग और लॉगिंग प्रक्रियाओं को स्ट्रीमलाइन करने के लिए करते हैं।

## कैसे करें:
Lua में, stderr पर लिखना `io.stderr:write()` फ़ंक्शन का उपयोग करके हासिल किया जा सकता है। यहाँ आप मानक त्रुटि पर एक सरल त्रुटि संदेश कैसे लिख सकते हैं:

```lua
io.stderr:write("Error: Invalid input.\n")
```

यदि आपको एक चर या कई डेटा टुकड़ों को आउटपुट करने की आवश्यकता है, तो उन्हें लिखने के फ़ंक्शन के भीतर जोड़ें:

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**stderr पर नमूना आउटपुट:**
```
Error: Invalid input.
```

अधिक जटिल परिदृश्यों के लिए, या बड़े अनुप्रयोगों के साथ काम करते समय, आप LuaLogging जैसी तृतीय-पक्ष लॉगिंग लाइब्रेरियों पर विचार कर सकते हैं। LuaLogging के साथ, आप विभिन्न गंतव्यों, सहित stderr पर लॉग्स को निर्देशित कर सकते हैं। यहाँ एक संक्षिप्त उदाहरण है:

सबसे पहले, LuaRocks का उपयोग करके LuaLogging इंस्टॉल करना सुनिश्चित करें:

```
luarocks install lualogging
```

फिर, LuaLogging का उपयोग करके stderr पर एक त्रुटि संदेश लिखने के लिए:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

इस दृष्टिकोण में आपके अप्लिकेशन में मानकीकृत लॉगिंग का लाभ है, साथ ही एक सरल API के माध्यम से लॉग स्तरों (जैसे ERROR, WARN, INFO) को सेट करने की अतिरिक्त लचीलापन के साथ।
