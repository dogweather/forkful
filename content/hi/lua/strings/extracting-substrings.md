---
date: 2024-01-20 17:46:40.453098-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.531119-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
