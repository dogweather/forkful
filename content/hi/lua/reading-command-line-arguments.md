---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
आदेश पंक्ति तर्क (`command line arguments`) उन मापदंडों को कहा जाता है जिन्हें आप अपने प्रोग्राम को शुरू करते समय पास करते हैं। प्रोग्रामर इसे संचालन के समय उपयोगकर्ता आदान-प्रदान और कस्टमाइज़ेशन को सशक्त करने के लिए करते हैं। 

## कैसे करें
आइए देखें कि कैसे Lua प्रोग्राम के आदेश पंक्ति तर्क को पढ़ा जाता है। 

```lua
for i, v in ipairs(arg) do
   print("Argument " .. i .. " : " .. v)
end
```
इसका आउटपुट कुछ ऐसा होगा अगर हम 'lua test.lua Hello World' चलाते हैं:

```lua
Argument 0 : test.lua
Argument 1 : Hello
Argument 2 : World
```

## गहराई से अध्ययन
आदेश पंक्ति के तर्कों का उपयोग प्राचीनिक समय से हो रहा है, जब उद्घोषणात्मक भाषाएँ पहली बार उद्भव हुई थीं। लुआ में, आप `arg` ग्लोबल तालिका का उपयोग करके इन तर्कों को पढ़ सकते हैं। वैकल्पिक रूप से, यदि आपको आदेश पंक्ति पार्सर की अधिक कन्ट्रोल की आवश्यकता है, तो आप कस्टम आदेश पंक्ति पार्सर लाइब्रेरियों का उपयोग कर सकते हैं। 

## संबंधित जानकारी
यदि आप और अधिक जानना चाहते हैं, तो निम्नलिखित साधनों की जाँच करें:
- विकिपीडिया पर आदेश पंक्ति तर्क: https://en.wikipedia.org/wiki/Command-line_interface#Arguments
- Lua में arg table का डॉक्यूमेंटेशन: https://www.lua.org/pil/20.1.html
-  कस्टम आदेश पंक्ति पार्सर: https://github.com/luarocks/argparse