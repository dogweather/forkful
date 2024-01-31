---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक डेटा सीरियलाइजेशन फॉर्मेट है जो ह्यूमन-रीडेबल है। प्रोग्रामर इसका इस्तेमाल कॉन्फ़िगरेशन फाइल्स, डेटा स्टोरेज, और डेटा एक्सचेंज के लिए करते हैं, क्योंकि इसे पढ़ना और लिखना आसान होता है।

## How to: (कैसे करें:)
Lua में YAML के साथ काम करने के लिए, `lyaml` लाइब्रेरी का इस्तेमाल करें। इसे इंस्टॉल करने के लिए `luarocks install lyaml` कमांड चलाएं।

YAML को पार्स करने के लिए:

```Lua
local lyaml = require('lyaml')

local yamlStr = [[
- name: John Doe
  age: 30
- name: Jane Doe
  age: 25
]]

local data = lyaml.load(yamlStr)
print(data[1].name)  -- Output: John Doe
```

YAML को जेनरेट करने के लिए:

```Lua
local dataToYaml = {
  {name = "John Doe", age = 30},
  {name = "Jane Doe", age = 25}
}

local yamlOutput = lyaml.dump(dataToYaml)
print(yamlOutput)
```

## Deep Dive (गहराई में जानकारी)
YAML (YAML Ain't Markup Language) 2001 में डेवलप किया गया था। इसके विकल्प में JSON और XML हैं, जो कि कमैन्यूनिटी में भी पॉपुलर हैं। YAML ह्यूमन-रीडेबल होने के साथ-साथ कंप्यूटरज़ के लिए भी आसानी से प्रोसेसेबल है। 'lyaml' मॉड्यूल Lua के लिए YAML प्रोसेसिंग का एक मजबूत टूल है, जो LibYAML लाइब्रेरी का उपयोग करता है। 

## See Also (और भी जानकारियां)
- YAML की अधिक जानकारी के लिए: https://yaml.org
- `lyaml` लाइब्रेरी डॉक्युमेंटेशन: https://github.com/gvvaughan/lyaml
- LuaRocks: https://luarocks.org
