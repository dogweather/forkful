---
date: 2024-01-20 17:36:01.110154-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) String\
  \ concatenation \u0915\u093E \u0915\u0949\u0928\u094D\u0938\u0947\u092A\u094D\u091F\
  \ \u092C\u0939\u0941\u0924 \u092A\u0941\u0930\u093E\u0928\u093E \u0939\u0948 \u0914\
  \u0930 \u0932\u0917\u092D\u0917 \u0938\u092D\u0940 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0932\u0948\u0902\u0917\u094D\u0935\u0947\
  \u091C\u0947\u0938 \u092E\u0947\u0902 \u092E\u093F\u0932\u0924\u093E \u0939\u0948\
  \u0964 Lua \u092E\u0947\u0902, \u0938\u092C\u0938\u0947 \u0938\u0930\u0932 \u0924\
  \u0930\u0940\u0915\u093E \u0939\u0948 `..`\u2026"
lastmod: '2024-04-05T22:51:07.213477-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) String concatenation\
  \ \u0915\u093E \u0915\u0949\u0928\u094D\u0938\u0947\u092A\u094D\u091F \u092C\u0939\
  \u0941\u0924 \u092A\u0941\u0930\u093E\u0928\u093E \u0939\u0948 \u0914\u0930 \u0932\
  \u0917\u092D\u0917 \u0938\u092D\u0940 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u093F\u0902\u0917 \u0932\u0948\u0902\u0917\u094D\u0935\u0947\u091C\u0947\
  \u0938 \u092E\u0947\u0902 \u092E\u093F\u0932\u0924\u093E \u0939\u0948\u0964 Lua\
  \ \u092E\u0947\u0902, \u0938\u092C\u0938\u0947 \u0938\u0930\u0932 \u0924\u0930\u0940\
  \u0915\u093E \u0939\u0948 `..` \u0911\u092A\u0930\u0947\u091F\u0930 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u091C\u094B\u0921\u093C\u0928\
  \u093E\u0964 \u0907\u0938 \u0924\u0930\u0940\u0915\u0947 \u0915\u0947 \u0905\u0932\
  \u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935\u094D\u0938 \u092E\u0947\u0902\
  \ `table.concat` \u0914\u0930 `string.format` \u091C\u0948\u0938\u0947 \u092B\u0902\
  \u0915\u094D\u0936\u0928\u094D\u0938 \u0906\u0924\u0947 \u0939\u0948\u0902, \u091C\
  \u094B \u0905\u0927\u093F\u0915 \u091C\u091F\u093F\u0932 \u0915\u0947\u0938\u0947\
  \u0938 \u092E\u0947\u0902 \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u094B\u0924\
  \u0947 \u0939\u0948\u0902\u0964 Lua \u0915\u0902\u0915\u0947\u091F\u0947\u0928\u0947\
  \u0936\u0928 \u092E\u0947\u0902 \u0907\u0902\u091F\u0930\u0928\u0932\u0940 Lua optimizer\
  \ \u091B\u094B\u091F\u0947 strings \u0915\u094B efficiently \u092E\u0930\u094D\u091C\
  \ \u0915\u0930\u0924\u093E \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u092C\u0921\
  \u093C\u0947 strings \u091C\u094B\u0921\u093C\u0924\u0947 \u0938\u092E\u092F \u092F\
  \u0939 \u0911\u092A\u0930\u0947\u0936\u0928 slow \u0939\u094B \u0938\u0915\u0924\
  \u093E \u0939\u0948\u0964 \u091C\u092C \u092C\u0939\u0941\u0924 \u0938\u093E\u0930\
  \u0947 strings \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u093E \u0939\u094B, \u0924\
  \u094B `table.concat` \u091C\u094D\u092F\u093E\u0926\u093E \u0924\u0947\u091C\u093C\
  \ \u0914\u0930 \u092E\u0947\u092E\u094B\u0930\u0940-\u0907\u092B\u0947\u0915\u094D\
  \u091F\u093F\u0935 \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
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
