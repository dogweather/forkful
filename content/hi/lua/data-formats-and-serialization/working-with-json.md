---
aliases:
- /hi/lua/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:13.043047-07:00
description: "Lua \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 \u091C\
  \u0947\u0938\u0928-\u0938\u094D\u0935\u0930\u0942\u092A\u093F\u0924 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B Lua \u091F\u0947\u092C\
  \u0932\u094D\u0938 \u092E\u0947\u0902 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u0914\u0930 \u0907\u0938\u0915\u0947 \u0935\u093F\u092A\u0930\u0940\
  \u0924, \u091C\u094B Lua \u0905\u0928\u0941\u092A\u094D\u0930\u092F\u094B\u0917\u094B\
  \u0902 \u0914\u0930 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\u0913\u0902 \u092F\
  \u093E \u092C\u093E\u0939\u094D\u092F APIs\u2026"
lastmod: 2024-02-18 23:09:03.614949
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902 JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 \u091C\
  \u0947\u0938\u0928-\u0938\u094D\u0935\u0930\u0942\u092A\u093F\u0924 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B Lua \u091F\u0947\u092C\
  \u0932\u094D\u0938 \u092E\u0947\u0902 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u0914\u0930 \u0907\u0938\u0915\u0947 \u0935\u093F\u092A\u0930\u0940\
  \u0924, \u091C\u094B Lua \u0905\u0928\u0941\u092A\u094D\u0930\u092F\u094B\u0917\u094B\
  \u0902 \u0914\u0930 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\u0913\u0902 \u092F\
  \u093E \u092C\u093E\u0939\u094D\u092F APIs\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
