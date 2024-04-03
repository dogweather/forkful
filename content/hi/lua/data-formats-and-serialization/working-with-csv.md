---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:50.842665-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Lua \u092E\u0947\u0902\
  , CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u093E \u0926\u0943\
  \u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u092D\u093E\u0937\u093E \u0926\u094D\
  \u0935\u093E\u0930\u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u093F\u090F\
  \ \u0917\u090F \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u092B\u093C\u093E\
  \u0907\u0932 IO \u0938\u0902\u091A\u093E\u0932\u0928\u094B\u0902 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948, \u0938\u0930\u0932\
  \ \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:52.588249-06:00'
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902, CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u0947\
  \ \u0915\u093E \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u092D\u093E\
  \u0937\u093E \u0926\u094D\u0935\u093E\u0930\u093E \u092A\u094D\u0930\u0926\u093E\
  \u0928 \u0915\u093F\u090F \u0917\u090F \u092C\u0941\u0928\u093F\u092F\u093E\u0926\
  \u0940 \u092B\u093C\u093E\u0907\u0932 IO \u0938\u0902\u091A\u093E\u0932\u0928\u094B\
  \u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\
  \u0948, \u0938\u0930\u0932 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u0947\
  \ \u0932\u093F\u090F \u092C\u093E\u0939\u0930\u0940 \u092A\u0941\u0938\u094D\u0924\
  \u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\
  \u0915\u0924\u093E \u0915\u0947 \u092C\u093F\u0928\u093E\u0964 \u0905\u0927\u093F\
  \u0915 \u091C\u091F\u093F\u0932 \u0938\u0902\u091A\u093E\u0932\u0928\u094B\u0902\
  \ \u0915\u0947 \u0932\u093F\u090F, \u091C\u0948\u0938\u0947 \u0915\u093F \u0935\u093F\
  \u0936\u0947\u0937 \u092E\u093E\u092E\u0932\u094B\u0902 \u0915\u094B \u0938\u0902\
  \u092D\u093E\u0932\u0928\u093E (\u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u0947\
  \ \u0932\u093F\u090F, \u092E\u093E\u0928\u094B\u0902 \u0915\u0947 \u092D\u0940\u0924\
  \u0930 \u0905\u0932\u094D\u092A\u0935\u093F\u0930\u093E\u092E), `lua-csv` \u091C\
  \u0948\u0938\u0947 \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\
  \u0947 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u0932\u093E\u092D\
  \u0926\u093E\u092F\u0915 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948\u0964\
  \n\n#."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

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
