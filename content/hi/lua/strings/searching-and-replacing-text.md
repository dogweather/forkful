---
date: 2024-01-20 17:58:21.968851-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Output."
lastmod: '2024-04-05T21:53:54.505726-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Output."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```Lua
local text = "Hello World! Welcome to Lua programming!"
local to_find = "World"
local replacement = "Lua"

local result = text:gsub(to_find, replacement)
print(result)
```
Output:
```
Hello Lua! Welcome to Lua programming!
```
यहां `gsub` फंक्शन `to_find` वाले शब्द को `replacement` से बदल देता है।

## Deep Dive (गहराई से जानकारी)
Lua में string मॉड्यूल का उपयोग करने से टेक्स्ट खोजना और बदलना आसान होता है। `gsub` एक उदाहरण है जिसका उपयोग रेगुलर एक्सप्रेशन पैटर्न्स के साथ भी किया जा सकता है। पहले कंप्यूटर साइंस में यह काम बहुत मुश्किल और लंबा होता था, पर Lua जैसी भाषाएं इसे आसानी से सीखने और लिखने योग्य बना देती हैं। कुछ भाषाएं `sed`, `awk`, या `perl` भी इसी प्रकार के काम में माहिर हैं। Lua में, `gsub` हाई-पर्फोरमेंस तथा लचीलापन के कारण चुना जाता है।

## See Also (अधिक जानकारी के लिए)
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- Online Lua Compiler / Web-based Lua playground for testing: [https://repl.it/languages/lua](https://repl.it/languages/lua)
