---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:13.043047-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Lua \u092E\u0947\u0902\
  \ JSON \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u092E\u093F\u0924\
  \ \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0936\u093E\u092E\u093F\
  \u0932 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964 \u0907\u0938\u0932\u093F\u090F\
  , `dkjson` \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\u0943\
  \u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\
  \u093E\u0932\u092F \u0939\u0948, \u091C\u093F\u0938\u0915\u093E \u0906\u092A JSON\u2026"
lastmod: '2024-03-13T22:44:52.586545-06:00'
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902 JSON \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\
  \u0902\u0917 \u0915\u0947 \u0932\u093F\u090F \u0915\u094B\u0908 \u0928\u093F\u0930\
  \u094D\u092E\u093F\u0924 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \ \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964 \u0907\
  \u0938\u0932\u093F\u090F, `dkjson` \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\
  \u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0939\u0948, \u091C\u093F\u0938\u0915\
  \u093E \u0906\u092A JSON \u090F\u0928\u094D\u0915\u094B\u0921\u093F\u0902\u0917\
  \ \u0914\u0930 \u0921\u093F\u0915\u094B\u0921\u093F\u0902\u0917 \u0915\u0947 \u0932\
  \u093F\u090F \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u0939\
  \u0932\u0947, \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\
  \u0947\u0902 \u0915\u093F `dkjson` \u0915\u094B \u0907\u0902\u0938\u094D\u091F\u0949\
  \u0932 \u0915\u0930\u0947\u0902, \u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u0947\
  \ \u0932\u093F\u090F, LuaRocks \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E\
  \ \u0938\u0947 (`luarocks install dkjson`), \u0914\u0930 \u092B\u093F\u0930 \u0928\
  \u0940\u091A\u0947 \u0926\u093F\u090F \u0917\u090F \u0909\u0926\u093E\u0939\u0930\
  \u0923\u094B\u0902 \u0915\u093E \u0905\u0928\u0941\u0938\u0930\u0923 \u0915\u0930\
  \u0947\u0902\u0964\n"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
Lua में JSON प्रोसेसिंग के लिए कोई निर्मित पुस्तकालय शामिल नहीं है। इसलिए, `dkjson` एक लोकप्रिय तृतीय-पक्ष पुस्तकालय है, जिसका आप JSON एन्कोडिंग और डिकोडिंग के लिए आसानी से उपयोग कर सकते हैं। पहले, सुनिश्चित करें कि `dkjson` को इंस्टॉल करें, उदाहरण के लिए, LuaRocks के माध्यम से (`luarocks install dkjson`), और फिर नीचे दिए गए उदाहरणों का अनुसरण करें।

### JSON को Lua टेबल में डिकोड करना
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Lua Programmer", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Error:", err)
else
  print("Name:", luaTable.name) -- आउटपुट: Name: Lua Programmer
  print("Age:", luaTable.age) -- आउटपुट: Age: 30
  print("Languages:", table.concat(luaTable.languages, ", ")) -- आउटपुट: Languages: Lua, JavaScript
end
```

### Lua टेबल को JSON में एन्कोड करना
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Lua Programmer",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

एन्कोडिंग के लिए नमूना आउटपुट:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Lua Programmer"
}
```

ये सरल उदाहरण Lua में JSON के साथ काम करने का तरीका प्रदर्शित करते हैं, जिससे विभिन्न वेब तकनीकों और बाह्य APIs के साथ Lua अनुप्रयोगों को आसानी से एकीकृत करना संभव होता है। याद रखें, जबकि इन उदाहरणों में `dkjson` का उपयोग किया गया है, `cjson` और `RapidJSON` जैसे अन्य पुस्तकालय भी आपके परियोजना की आवश्यकताओं के आधार पर उपयुक्त विकल्प हो सकते हैं।
