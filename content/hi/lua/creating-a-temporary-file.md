---
title:                "अस्थायी फाइल बनाना"
aliases:
- hi/lua/creating-a-temporary-file.md
date:                  2024-01-20T17:40:49.543621-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Temporary file(अस्थायी फाइल) वह फाइल होती है जो डाटा को अस्थायी रूप से स्टोर करती है. Programmers इसे इस्तेमाल करते हैं क्योंकि यह मेमोरी management में मदद करता है और ऐसे डाटा को संभालने के लिए जो केवल short time के लिए जरूरी होता है.

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
