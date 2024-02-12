---
title:                "CSV के साथ काम करना"
aliases:
- /hi/lua/working-with-csv/
date:                  2024-02-03T19:21:50.842665-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma-Separated Values) फ़ाइलों के साथ कार्य करना शामिल होता है पाठ डेटा को पार्सिंग और उत्पन्न करने में, जो पंक्तियों और कॉलमों में व्यवस्थित होता है, विभिन्न मानों को अलग करने के लिए अल्पविरामों का उपयोग करके। प्रोग्रामर अक्सर यह प्रक्रिया अलग-अलग अनुप्रयोगों, डेटाबेस, या डेटा प्रसंस्करण और विश्लेषण कार्यों के बीच डेटा के आदान-प्रदान की सुविधा के लिए करते हैं, CSV के व्यापक समर्थन और सरलता के कारण।

## कैसे करें:

Lua में, CSV फ़ाइलों के साथ कार्य करने का दृष्टिकोण भाषा द्वारा प्रदान किए गए बुनियादी फ़ाइल IO संचालनों का उपयोग करता है, सरल कार्यों के लिए बाहरी पुस्तकालयों की आवश्यकता के बिना। अधिक जटिल संचालनों के लिए, जैसे कि विशेष मामलों को संभालना (उदाहरण के लिए, मानों के भीतर अल्पविराम), `lua-csv` जैसे तीसरे पक्ष के पुस्तकालयों का उपयोग करना लाभदायक हो सकता है।

### एक CSV फ़ाइल पढ़ना
यहाँ एक सरल उदाहरण है एक CSV फ़ाइल को पंक्ति दर पंक्ति पढ़ने के लिए, प्रत्येक पंक्ति को अल्पविराम विभाजक के आधार पर मानों में विभाजित करना।

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**नमूना आउटपुट** (एक 'example.csv' के लिए जिसमें सामग्री है "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### एक CSV फ़ाइल लिखना
एक CSV फ़ाइल उत्पन्न करने के लिए, आप सरलता से कॉमा-विभाजित मानों के साथ स्ट्रिंग्स बनाएं और उन्हें पंक्ति दर पंक्ति फ़ाइल में लिखें।

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

यह निर्दिष्ट डेटा के साथ एक `output.csv` फ़ाइल बनाएगा (या अधिलेखित करेगा)।

### lua-csv का उपयोग करना
उद्धरण चिह्न और एस्केप कैरेक्टरों के साथ समर्थन सहित अधिक उन्नत CSV संचालन के लिए, `lua-csv` पुस्तकालय एक मजबूत विकल्प है।

पहले, इसे LuaRocks का उपयोग करके स्थापित करें:
```shell
luarocks install lua-csv
```

तब, एक CSV फ़ाइल पढ़ना उतना ही सरल हो जाता है:

```lua
local csv = require("csv")

-- फ़ाइल से पढ़ना
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

और सही उद्धरण और एस्केपिंग के साथ एक CSV में लिखना:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

यह दृष्टिकोण स्वचालित रूप से मानों के भीतर अल्पविरामों और उद्धरण चिह्नों जैसी जटिलताओं को संभालता है।
