---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:57:32.698606-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्गुमेंट्स पढ़ना, यूज़र से डाटा इनपुट पाने का एक तरीका है। प्रोग्रामर्स इसे इस्तेमाल करते हैं ताकि वे फ्लेक्सिबल प्रोग्राम बना सकें जो अलग-अलग सिचुएशन्स के लिए कस्टमाइज़ हो सकें।

## How to (कैसे करें):
Lua में कमांड लाइन आर्गुमेंट्स पढ़ना सीधा है। यहाँ एक उदाहरण है:

```Lua
-- सेव करें जैसे: hello.lua

-- पहला आर्गुमेंट {_G.arg[0]} होता है जो स्क्रिप्ट का नाम होता है
print("स्क्रिप्ट का नाम:", _G.arg[0])

-- बाकी आर्गुमेंट्स {_G.arg[1], _G.arg[2], ...}
if #_G.arg > 0 then
    print("नमस्ते", _G.arg[1])
else
    print("नमस्ते, दुनिया!")
end
```

कमांड लाइन पर चलाएँ:

```
> lua hello.lua Sita
स्क्रिप्ट का नाम: hello.lua
नमस्ते Sita
```

## Deep Dive (गहराई से जानकारी):
कमांड लाइन आर्गुमेंट्स की क्षमता पुराने डेवेलपमेंट सिस्टम्स से आई है जहां इंटरेक्टिव इनपुट्स सीमित थे। लुआ में, `_G.arg` table इंडेक्स `0` से शुरू होता है जो स्क्रिप्ट का पूरा पाथ देता है, फिर `1` से उसके बाद के आर्गुमेंट्स मिलते हैं। इसके अलावा, लुआ में आर्गुमेंट्स लेने के दूसरे तरीके भी हैं जैसे कि `io.read()` या लाइब्रेरीज़ जैसे `lapp` और `argparse`। पर `_G.arg` का इस्तेमाल सबसे आम है क्योंकि यह सिंपल और पोर्टेबल होता है।

## See Also (और भी जानकारी):
- Lua ऑफिसियल डॉक्यूमेंटेशन: [https://www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
- एक्सटर्नल आर्गुमेंट प्रोसेसिंग लाइब्रेरी `Penlight`: [https://github.com/lunarmodules/Penlight](https://github.com/lunarmodules/Penlight)
- लुआ `lapp` लाइब्रेरी फॉर आर्गुमेंट पार्सिंग: [https://stevedonovan.github.io/Penlight/api/libraries/pl.lapp.html](https://stevedonovan.github.io/Penlight/api/libraries/pl.lapp.html)
