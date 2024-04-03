---
date: 2024-01-20 17:36:01.110154-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.535520-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to: (कैसे करें:)
```Lua
-- सिंपल कोड एक्जाम्पल
local welcome = "Namaste"
local name = "Duniya"
local message = welcome .. ", " .. name .. "!"
print(message) -- Namaste, Duniya!
```

```Lua
-- वेरिएबल्स और लूप्स के साथ कोड एक्जाम्पल
local fruits = {"seb", "kela", "aam"}
local list = "Fruit List: "
for i, fruit in ipairs(fruits) do
    list = list .. fruit
    if i < #fruits then
        list = list .. ", "
    end
end
print(list) -- Fruit List: seb, kela, aam
```

```Lua
-- string.format का उपयोग करना
local temp = 25
local weather = string.format("Aaj ka taapman: %d degree Celsius hai.", temp)
print(weather) -- Aaj ka taapman: 25 degree Celsius hai.
```

## Deep Dive (गहराई में जानकारी)
String concatenation का कॉन्सेप्ट बहुत पुराना है और लगभग सभी प्रोग्रामिंग लैंग्वेजेस में मिलता है। Lua में, सबसे सरल तरीका है `..` ऑपरेटर का उपयोग करके स्ट्रिंग्स को जोड़ना। इस तरीके के अल्टरनेटिव्स में `table.concat` और `string.format` जैसे फंक्शन्स आते हैं, जो अधिक जटिल केसेस में उपयोगी होते हैं।

Lua कंकेटेनेशन में इंटरनली Lua optimizer छोटे strings को efficiently मर्ज करता है, लेकिन बड़े strings जोड़ते समय यह ऑपरेशन slow हो सकता है। जब बहुत सारे strings को जोड़ना हो, तो `table.concat` ज्यादा तेज़ और मेमोरी-इफेक्टिव होता है।

## See Also (देखें भी)
- Lua 5.4 reference manual on string concatenation: [https://www.lua.org/manual/5.4/manual.html#3.4.6](https://www.lua.org/manual/5.4/manual.html#3.4.6)
- Lua string library for advanced operations: [https://www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
