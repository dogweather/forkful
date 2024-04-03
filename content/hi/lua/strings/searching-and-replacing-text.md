---
date: 2024-01-20 17:58:21.968851-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0916\u094B\u091C\u0928\u093E\
  \ \u0914\u0930 \u092C\u0926\u0932\u0928\u093E \u0939\u0948 \u0924\u093E\u0930 (string)\
  \ \u092E\u0947\u0902 \u0935\u093F\u0936\u0947\u0937 \u0905\u0915\u094D\u0937\u0930\
  \ \u092F\u093E \u0936\u092C\u094D\u0926\u094B\u0902 \u0915\u094B \u0922\u0942\u0901\
  \u0922 \u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\u090F \u0938\u0947\
  \ \u092C\u0926\u0932 \u0926\u0947\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0907\u0938\u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0915\u094B\u0921 \u092E\u0947\u0902\
  \ \u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.525355-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0916\u094B\u091C\u0928\u093E\
  \ \u0914\u0930 \u092C\u0926\u0932\u0928\u093E \u0939\u0948 \u0924\u093E\u0930 (string)\
  \ \u092E\u0947\u0902 \u0935\u093F\u0936\u0947\u0937 \u0905\u0915\u094D\u0937\u0930\
  \ \u092F\u093E \u0936\u092C\u094D\u0926\u094B\u0902 \u0915\u094B \u0922\u0942\u0901\
  \u0922 \u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\u090F \u0938\u0947\
  \ \u092C\u0926\u0932 \u0926\u0947\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0907\u0938\u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0915\u094B\u0921 \u092E\u0947\u0902\
  \ \u092F\u093E \u0921\u093E\u091F\u093E \u092E\u0947\u0902 \u0938\u0941\u0927\u093E\
  \u0930 \u092F\u093E \u0905\u092A\u0921\u0947\u091F\u094D\u0938 \u0915\u0940 \u095B\
  \u0930\u0942\u0930\u0924 \u0939\u094B\u0924\u0940 \u0939\u0948\u0964."
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
