---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/lua/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:04.722789-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक स्ट्रिंग को लोअर केस में बदलने का मतलब है इसे सभी छोटे अक्षरों में बदलना। यह जरूरी होता है जब हमें केस इंसेंसिटिव सर्च या सॉर्टिंग करनी हो।

## How to: (कैसे करें:)
```Lua
local original_string = "नमस्ते, दुनिया!"
local lower_case_string = original_string:lower()

print(lower_case_string)  -- नमस्ते, दुनिया! को लोअर केस में प्रिंट करेगा
```
उदाहरण आउटपुट:
```
नमस्ते, दुनिया!
```

## Deep Dive (गहन जानकारी)
Lua में स्ट्रिंग को लोअर केस में बदलना सिम्पल है, क्योंकि भाषा डिजाइन से ही आसान और सीधी है। `string.lower()` फंक्शन 5.0 से ही उपलब्ध है और ये UTF-8 एन्कोडिंग का भी समर्थन करता है, जो अंतरराष्ट्रीय अक्षरों के साथ काम करने में सहायक है।

हालांकि कुछ खास केसेज में, आप `string.lower()` के बजाय पैटर्न मैचिंग और गुस्टोम फंक्शन भी बना सकते हैं उन भाषाओं के लिए जहां स्पेशल केस रूल्स हों।

## See Also (यह भी देखें)
- Lua 5.4 रेफरेंस मैन्युअल: https://www.lua.org/manual/5.4/
- Lua string patterns: https://www.lua.org/pil/20.2.html
- Lua GitHub repository: https://github.com/lua/lua
