---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक डेटा प्रारूप है जिसे डेटा आदान-प्रदान के लिए इस्तेमाल किया जाता है। प्रोग्रामर इसे आसान और पढ़ने में सरल होने के कारण, वेब API और कॉन्फ़िगरेशन फ़ाइलों में इस्तेमाल करते हैं।

## How to: (कैसे करें)
Lua में JSON को पढ़ने और लिखने के लिए आपको एक बाहरी लाइब्रेरी जैसे `dkjson` का इस्तेमाल करना पड़ेगा। यहां पर एक उदाहरण है:

```Lua
local json = require("dkjson")

-- JSON को Lua टेबल में परिवर्तित करना
local jsonString = '{"name": "राम", "age": 25, "is_student": false}'
local luaTable, pos, err = json.decode(jsonString, 1, nil)
if err then
  print ("एरर:", err)
else
  print ("नाम:", luaTable.name) -- आउटपुट: राम
end

-- Lua टेबल को JSON में परिवर्तित करना
local newLuaTable = { name = "राम", age = 25, is_student = false }
local newJsonString = json.encode(newLuaTable)
print(newJsonString) -- आउटपुट: {"name":"राम","age":25,"is_student":false}
```

## Deep Dive (गहराई से जानकारी)
JSON 2001 में वेब डेटा के लिए एक आसानी से पढ़े और लिखे जाने वाले प्रारूप के रूप में उत्पन्न हुआ। XML और YAML जैसे अन्य प्रारूप हैं लेकिन JSON की सरलता के कारण यह ज्यादा लोकप्रिय है। Lua में JSON का इंप्लीमेंटेशन बाहरी लाइब्रेरी के माध्यम से होता है और `dkjson` इसके लिए एक विश्वसनीय विकल्प है।

## See Also (यह भी देखें)
- [JSON.org](https://www.json.org/json-en.html): JSON के बारे में और जानकारी।
- [GitHub - dkjson](https://github.com/LuaDist/dkjson): Lua के लिए `dkjson` लाइब्रेरी।
- [Programming in Lua](https://www.lua.org/pil/): Lua भाषा सीखने के लिए आधिकारिक किताब।
- [Lua Users Wiki - JSON Modules](http://lua-users.org/wiki/JsonModules): JSON प्रोसेसिंग के लिए Lua मॉड्यूल्स।