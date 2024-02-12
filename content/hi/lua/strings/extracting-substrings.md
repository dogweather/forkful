---
title:                "सबस्ट्रिंग्स निकालना"
aliases: - /hi/lua/extracting-substrings.md
date:                  2024-01-20T17:46:40.453098-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring का मतलब होता है एक बड़ी स्ट्रिंग से छोटे टुकड़े निकालना। Programmers इसका इस्तेमाल खास जानकारी प्रोसेस करने, डेटा वैलिडेट और पैटर्न मैच करने के लिए करते हैं।

## How to: (कैसे करें:)
```Lua
-- एक बेसिक स्ट्रिंग
local originalString = "नमस्ते, Lua का दुनिया में स्वागत है!"

-- Substring निकालना: string.sub function
local substring = string.sub(originalString, 9, 11)
print(substring) -- Output: Lua

-- शुरू से कुछ characters निकालना
local startString = string.sub(originalString, 1, 7)
print(startString) -- Output: नमस्ते,

-- आखिर से कुछ characters छोड़ देना
local endIndex = string.len(originalString)
local endString = string.sub(originalString, endIndex - 12, endIndex)
print(endString) -- Output: स्वागत है!
```

## Deep Dive (गहराई से जानकारी):
Lua में substrings निकालने का अपना एक महत्व है। पहले के समय में जब मेमोरी और प्रोसेसिंग पावर सीमित थी, तब डेटा को सूक्ष्मता से हैंडल करना महत्वपूर्ण था। Lua में `string.sub` यही काम करता है और यह Lua 5.1 से मौजूद है।

विकल्प के तौर पर, कभी-कभार pattern matching (`string.match`, `string.gmatch`) या regex-लाइब्रेरी जैसे `lrexlib` का इस्तेमाल होता है।

Implementation details में, यह जानना जरूरी है कि `string.sub` zero-based इंडेक्सिंग नहीं अपनाता, यानी counting 1 से शुरू होती है। और Lua UTF-8 को default में पूरा support नहीं करता, इसलिए Unicode strings के साथ काम करते वक्त एक्स्ट्रा केयर लेनी पड़ती है।

## See Also (और देखें):
- Lua 5.4 Reference Manual: [string.sub]: https://www.lua.org/manual/5.4/manual.html#pdf-string.sub
- Programming in Lua (ऑफिशियल बुक): https://www.lua.org/pil/contents.html
- Lua Users Wiki (pattern matching): http://lua-users.org/wiki/PatternsTutorial
- Unicode Considerations in Lua: http://lua-users.org/wiki/LuaUnicode
