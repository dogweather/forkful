---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:21.968851-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट खोजना और बदलना है तार (string) में विशेष अक्षर या शब्दों को ढूँढ कर उन्हें नए से बदल देना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि अक्सर कोड में या डाटा में सुधार या अपडेट्स की ज़रूरत होती है।

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
