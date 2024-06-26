---
date: 2024-01-20 17:40:49.543621-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Temporary\
  \ files \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u092A\u0939\
  \u0932\u0947 \u0924\u092C \u0939\u0941\u0908 \u091C\u092C mainframes \u092A\u0930\
  \ \u092E\u0932\u094D\u091F\u0940-\u092F\u0942\u095B\u0930 environments \u092E\u0947\
  \u0902 concurrently \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u095B\u0930\u0942\
  \u0930\u0940 \u0939\u094B \u0917\u092F\u093E. \u0935\u0947 buffer \u0915\u0947\u2026"
lastmod: '2024-04-05T22:51:07.261900-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Temporary files \u0915\
  \u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u092A\u0939\u0932\u0947\
  \ \u0924\u092C \u0939\u0941\u0908 \u091C\u092C mainframes \u092A\u0930 \u092E\u0932\
  \u094D\u091F\u0940-\u092F\u0942\u095B\u0930 environments \u092E\u0947\u0902 concurrently\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u095B\u0930\u0942\u0930\u0940 \u0939\
  \u094B \u0917\u092F\u093E."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें:)
```Lua
local os = require("os")

-- Temporary फाइल बनाएं और नाम प्राप्त करें
local temp_filename = os.tmpname()

-- उस फाइल को खोलें और कुछ डाटा लिखें
local temp_file = io.open(temp_filename, "w")
temp_file:write("हैलो, यह कुछ अस्थायी डाटा है!")
temp_file:close()

-- फाइल खोलें और डाटा पढ़ें
temp_file = io.open(temp_filename, "r")
print(temp_file:read("*a"))  -- Output: हैलो, यह कुछ अस्थायी डाटा है!
temp_file:close()

-- अस्थायी फाइल को हटाएं
os.remove(temp_filename)
```

## Deep Dive (गहराई से जानकारी):
Temporary files की आवश्यकता पहले तब हुई जब mainframes पर मल्टी-यूज़र environments में concurrently काम करना ज़रूरी हो गया. वे buffer के रूप में काम करते हैं, corrupt data को रोकते हैं, और data loss को minimize करते हैं अगर प्रोग्राम फ़ौरन बंद हो जाये. Lua में `os.tmpname()` function एक unique temporary file name generate करता है, जबकि `io.open()` का इस्तेमाल file को खोलने और डाटा पढ़ने/लिखने के लिए होता है. Alternatives में filesystem libraries जैसे Luvit का libuv, और luaposix हैं, जो ज्यादा advanced API प्रदान करते हैं.

## See Also (अन्य संसाधन):
- Lua File I/O Documentation: https://www.lua.org/pil/21.1.html
- Lua 'os' Library Reference: https://www.lua.org/manual/5.4/manual.html#6.9
- LuaTemp (एक तृतीय-पक्ष library for प्रबंधन temporary files in Lua): https://github.com/LuaDist/luatemp
