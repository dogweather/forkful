---
date: 2024-01-20 17:46:40.453098-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Lua \u092E\
  \u0947\u0902 substrings \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u093E\
  \ \u0905\u092A\u0928\u093E \u090F\u0915 \u092E\u0939\u0924\u094D\u0935 \u0939\u0948\
  \u0964 \u092A\u0939\u0932\u0947 \u0915\u0947 \u0938\u092E\u092F \u092E\u0947\u0902\
  \ \u091C\u092C \u092E\u0947\u092E\u094B\u0930\u0940 \u0914\u0930 \u092A\u094D\u0930\
  \u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u092A\u093E\u0935\u0930 \u0938\u0940\
  \u092E\u093F\u0924 \u0925\u0940, \u0924\u092C \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u0938\u0942\u0915\u094D\u0937\u094D\u092E\u0924\u093E \u0938\u0947 \u0939\u0948\
  \u0902\u0921\u0932\u2026"
lastmod: '2024-04-05T22:51:07.208564-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Lua \u092E\u0947\u0902\
  \ substrings \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u093E \u0905\u092A\
  \u0928\u093E \u090F\u0915 \u092E\u0939\u0924\u094D\u0935 \u0939\u0948\u0964 \u092A\
  \u0939\u0932\u0947 \u0915\u0947 \u0938\u092E\u092F \u092E\u0947\u0902 \u091C\u092C\
  \ \u092E\u0947\u092E\u094B\u0930\u0940 \u0914\u0930 \u092A\u094D\u0930\u094B\u0938\
  \u0947\u0938\u093F\u0902\u0917 \u092A\u093E\u0935\u0930 \u0938\u0940\u092E\u093F\
  \u0924 \u0925\u0940, \u0924\u092C \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u0942\
  \u0915\u094D\u0937\u094D\u092E\u0924\u093E \u0938\u0947 \u0939\u0948\u0902\u0921\
  \u0932 \u0915\u0930\u0928\u093E \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\
  \u094D\u0923 \u0925\u093E\u0964 Lua \u092E\u0947\u0902 `string.sub` \u092F\u0939\
  \u0940 \u0915\u093E\u092E \u0915\u0930\u0924\u093E \u0939\u0948 \u0914\u0930 \u092F\
  \u0939 Lua 5.1 \u0938\u0947 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\u0964 \u0935\
  \u093F\u0915\u0932\u094D\u092A \u0915\u0947 \u0924\u094C\u0930 \u092A\u0930, \u0915\
  \u092D\u0940-\u0915\u092D\u093E\u0930 pattern matching (`string.match`, `string.gmatch`)\
  \ \u092F\u093E regex-\u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u091C\
  \u0948\u0938\u0947 `lrexlib` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 Implementation details \u092E\
  \u0947\u0902, \u092F\u0939 \u091C\u093E\u0928\u0928\u093E \u091C\u0930\u0942\u0930\
  \u0940 \u0939\u0948 \u0915\u093F `string.sub` zero-based \u0907\u0902\u0921\u0947\
  \u0915\u094D\u0938\u093F\u0902\u0917 \u0928\u0939\u0940\u0902 \u0905\u092A\u0928\
  \u093E\u0924\u093E, \u092F\u093E\u0928\u0940 counting 1 \u0938\u0947 \u0936\u0941\
  \u0930\u0942 \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u0914\u0930 Lua UTF-8\
  \ \u0915\u094B default \u092E\u0947\u0902 \u092A\u0942\u0930\u093E support \u0928\
  \u0939\u0940\u0902 \u0915\u0930\u0924\u093E, \u0907\u0938\u0932\u093F\u090F Unicode\
  \ strings \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0924\u0947\
  \ \u0935\u0915\u094D\u0924 \u090F\u0915\u094D\u0938\u094D\u091F\u094D\u0930\u093E\
  \ \u0915\u0947\u092F\u0930 \u0932\u0947\u0928\u0940 \u092A\u0921\u093C\u0924\u0940\
  \ \u0939\u0948\u0964."
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
