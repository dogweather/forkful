---
date: 2024-01-20 17:47:47.420811-07:00
description: "String \u0915\u0940 length \u091C\u093E\u0928\u0928\u0947 \u0938\u0947\
  \ \u092A\u0924\u093E \u091A\u0932\u0924\u093E \u0939\u0948 \u0915\u093F \u0909\u0938\
  \u092E\u0947\u0902 \u0915\u093F\u0924\u0928\u0947 characters \u0939\u0948\u0902\u0964\
  \ Programmers \u0907\u0938\u0947 \u091C\u094D\u092F\u093E\u0926\u093E\u0924\u0930\
  \ data validation, processing text, \u092F\u093E loops \u0915\u0947 \u0938\u0939\
  \u0940 execution\u2026"
lastmod: '2024-03-13T22:44:52.534181-06:00'
model: gpt-4-1106-preview
summary: "String \u0915\u0940 length \u091C\u093E\u0928\u0928\u0947 \u0938\u0947 \u092A\
  \u0924\u093E \u091A\u0932\u0924\u093E \u0939\u0948 \u0915\u093F \u0909\u0938\u092E\
  \u0947\u0902 \u0915\u093F\u0924\u0928\u0947 characters \u0939\u0948\u0902\u0964\
  \ Programmers \u0907\u0938\u0947 \u091C\u094D\u092F\u093E\u0926\u093E\u0924\u0930\
  \ data validation, processing text, \u092F\u093E loops \u0915\u0947 \u0938\u0939\
  \u0940 execution \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

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
