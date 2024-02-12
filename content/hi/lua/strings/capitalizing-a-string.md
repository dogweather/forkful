---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases:
- /hi/lua/capitalizing-a-string.md
date:                  2024-02-03T19:06:41.911621-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग की पूंजीकरण का मतलब है वाक्य में प्रत्येक शब्द के पहले अक्षर को बड़ा अक्षर (अपरकेस) में बदलना, साथ ही बाकी सभी को छोटे अक्षर (लोअरकेस) में रखना। यह तकनीक अक्सर पाठ को अधिक पेशेवर या पठनीय आउटपुट के लिए फॉर्मेट करने में इस्तेमाल की जाती है, जैसे कि शीर्षक तैयार करना या उपयोगकर्ता के इनपुट को प्रदर्शन के लिए तैयार करना।

## कैसे करें:
Lua में स्ट्रिंग की पूंजीकरण के लिए बिल्ट-इन फंक्शन नहीं है, लेकिन आप आसानी से बेसिक स्ट्रिंग मेनिपुलेशन फंक्शंस का उपयोग करके यह कार्य कर सकते हैं। यहाँ एक सिंगल शब्द के पहले अक्षर को पूंजीकृत करने के लिए एक सरल फंक्शन है:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- आउटपुट: Hello
```

एक वाक्य में प्रत्येक शब्द को पूंजीकृत करने के लिए, आप वाक्य को शब्दों में विभाजित कर सकते हैं, प्रत्येक को पूंजीकृत करें, और फिर उन्हें फिर से जोड़ें:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- आउटपुट: Hello World From Lua
```

यदि आप किसी ऐसी परियोजना पर कार्य कर रहे हैं जहां प्रदर्शन महत्वपूर्ण है और आपको अधिक उन्नत स्ट्रिंग मेनिपुलेशन क्षमताओं की आवश्यकता महसूस होती है, तो `Penlight` जैसी थर्ड-पार्टी लाइब्रेरी का उपयोग करने पर विचार करें। Penlight अन्य उपयोगिताओं के साथ-साथ अधिक बहुमुखी स्ट्रिंग हैंडलिंग फंक्शंस के साथ Lua को बढ़ाता है:

```lua
-- मान लिया जाता है कि Penlight इंस्टॉल है:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- आउटपुट: Hello lua users

-- ध्यान दें: Penlight की capitalized फंक्शन केवल पहली शब्द को पूंजीकृत करती है।
-- हर शब्द को पूंजीकृत करने के लिए, आपको अभी भी एक कस्टम समाधान लागू करने की आवश्यकता होगी या अन्य लाइब्रेरीज का पता लगाना होगा।
```
