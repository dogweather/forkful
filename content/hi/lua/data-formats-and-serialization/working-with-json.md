---
title:                "JSON के साथ काम करना"
date:                  2024-02-03T19:24:13.043047-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Lua में JSON के साथ काम करना शामिल है जेसन-स्वरूपित स्ट्रिंग्स को Lua टेबल्स में पार्स करना और इसके विपरीत, जो Lua अनुप्रयोगों और वेब सेवाओं या बाह्य APIs के बीच आसानी से डेटा आदान-प्रदान को सक्षम बनाता है। प्रोग्रामर इसे करते हैं ताकि JSON के हल्के और आसानी से पार्स किए जाने वाले स्वरूप का लाभ उठाकर डेटा संग्रहण, कॉन्फिगरेशन, या API संचार के लिए कुशलता प्राप्त कर सकें।

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
