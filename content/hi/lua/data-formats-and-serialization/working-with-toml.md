---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:26:18.042944-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ कार्य करना लुआ (Lua) के साथ TOML (टॉम की स्पष्ट, मिनिमल लैंग्वेज) डेटा का पार्सिंग और उत्पन्न करना शामिल है। प्रोग्रामर्स कॉन्फ़िग फाइलों के लिए TOML का उपयोग करते हैं क्योंकि इसकी पठनीयता और सरल सिंटैक्स डेटा संरचना में आसानी से अनुवादित होता है।

## कैसे:
पहले, सुनिश्चित करें कि आपका लुआ वातावरण में एक TOML पार्सर है। हम इस उदाहरण के लिए `lua-toml` का उपयोग करेंगे।

```Lua
local toml = require("toml")

-- पार्स TOML स्ट्रिंग
local toml_data = [[
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Example"

-- TOML स्ट्रिंग उत्पन्न करें
local table_data = {
  title = "TOML Example",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

नमूना आउटपुट:
```
TOML Example
```

## गहराई में अध्ययन
TOML को 2013 में टॉम प्रेस्टन-वर्नर द्वारा XML और YAML जैसी अन्य डेटा सीरियलाइज़ेशन भाषाओं के एक विकल्प के रूप में बनाया गया था, जो कॉन्फ़िगरेशन डेटा को प्रस्तुत करने के लिए एक अधिक सीधा प्रारूप प्रदान करता है। जबकि JSON सर्वव्यापी है, इसका सिंटैक्स कॉन्फ़िग फ़ाइलों के लिए बोझिल हो सकता है। TOML मनुष्यों के लिए एक स्पष्ट सिंटैक्स के साथ चमकता है, जो .ini फाइलों के समान दिखता है लेकिन नेस्टिंग क्षमताओं और डेटा प्रकारों के साथ।

TOML के विकल्पों में JSON, YAML, और XML शामिल हैं। हालाँकि, TOML विशेष रूप से कॉन्फ़िग के लिए डिज़ाइन किया गया है और अर्ग्यूमेंटली YAML से सरल, कॉन्फ़िग उद्देश्यों के लिए JSON से अधिक पठनीय, और XML से कम वर्बोज़ है।

लुआ में TOML संभालने को लागू करने की आमतौर पर एक तृतीय-पक्ष लाइब्रेरी की आवश्यकता होती है। प्रदर्शन और सुविधाओं में विविधता हो सकती है, मूल पार्सिंग से लेकर पूर्ण सीरियलाइज़ेशन सपोर्ट तक। बड़ी कॉन्फ़िग फाइलों से निपटने या बार-बार पठन/लेखन संचालनों पर विचार करते समय, लाइब्रेरी के प्रदर्शन और नवीनतम TOML संस्करण के साथ अनुपालन को ध्यान में रखें।

## देखें भी
- TOML विनिर्देशन: https://toml.io/en/
- `lua-toml` लाइब्रेरी: https://github.com/jonstoler/lua-toml
- डेटा सीरियलाइज़ेशन प्रारूपों की तुलना: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
