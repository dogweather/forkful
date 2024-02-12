---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:47:47.420811-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String की length जानने से पता चलता है कि उसमें कितने characters हैं। Programmers इसे ज्यादातर data validation, processing text, या loops के सही execution के लिए करते हैं।

## How to: (कैसे करें:)
```Lua
-- एक simple string
local greeting = "नमस्ते"
-- Length पाने के लिए # operator का उपयोग करें
local length = #greeting
print(length)  -- Output: 18
```
ध्यान दें कि Unicode characters की वजह से length ज्यादा दिख सकती है।

## Deep Dive (गहराई से जानकारी)
Lua में string की length जानने के लिए `#` operator इस्तेमाल होता है। पहले वर्ज़न्स में, strings के लिए `strlen` जैसे functions हो सकते थे, पर Lua में यह सीधा और सरल है।

Alternatives में Unicode-aware libraries हैं जो accurate string lengths देते हैं, खासकर multibyte characters के साथ। उदाहरण के लिए, `utf8.len()` function का उपयोग सटीक length ढूंढने के लिए होता है।

Implementation details की बात करें तो, '#' operator efficiently implemented होता है क्योंकि Lua strings immutable होती हैं और उनकी length storage में पहले से stored होती है।

## See Also (और जानकारी के लिए)
- Lua's Reference Manual for strings: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- Online tutorial for Lua strings: [www.tutorialspoint.com/lua/lua_strings.htm](https://www.tutorialspoint.com/lua/lua_strings.htm)
