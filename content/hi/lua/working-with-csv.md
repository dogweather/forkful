---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV यानी Comma-Separated Values, एक साधारण फाइल फॉर्मेट है जो टेबुलर डेटा को स्टोर करता है। प्रोग्रामर्स इसे इसलिए इस्तेमाल करते हैं क्योंकि यह सरल है और डेटाबेस या एक्सेल शीट्स के साथ आसानी से काम करता है।

## How to: (कैसे करें:)
```Lua
-- CSV फाइल पढ़ना
local function ReadCSV(filePath)
    local file = io.open(filePath, "r") -- फाइल खोल रहे हैं
    if not file then return nil, "Unable to open file" end

    local data = {} -- डेटा को स्टोर करने के लिए एक टेबल
    for line in file:lines() do
        local row = {}
        for value in line:gmatch("[^,]+") do
            table.insert(row, value)
        end
        table.insert(data, row)
    end
    file:close()
    return data
end

-- CSV फाइल लिखना
local function WriteCSV(filePath, data)
    local file = io.open(filePath, "w") -- फाइल खोल रहे हैं
    if not file then return false, "Unable to open file" end

    for _, row in ipairs(data) do
        file:write(table.concat(row, ",") .. "\n")
    end
    file:close()
    return true
end

-- उपयोग (Usage)
local filePath = "example.csv"

-- CSV पढ़ना
local data, err = ReadCSV(filePath)
if data then
    for i, row in ipairs(data) do
        print("Row " .. i .. ":")
        for j, value in ipairs(row) do
            print(" ", value)
        end
    end
end

-- CSV लिखना
local success, err = WriteCSV("new_example.csv", {{"ID", "Name", "Age"}, {1, "Aman", 30}, {2, "Priya", 25}})
if not success then
    print(err)
end
```

## Deep Dive (गहराई में जानकारी)
CSV का इस्तेमाल 1970 के दशक से हो रहा है। अन्य फॉर्मेट्स जैसे JSON या XML भी डेटा को स्टोर करने के लिए होते हैं, पर CSV की सादगी इसे आकर्षक बनाती है। Lua में CSV का हैंडल करना प्रत्यक्ष है पर यदि उदाहरण के लिए डाटा में कॉमा या न्यूलाइन्स हैं, तो उसे कोट्स में wrap करना या अन्य एस्केपिंग तकनीक का इस्तेमाल करना ज़रूरी होता है।

## See Also (इसे भी देखें)
- Lua मैनुअल: [https://www.lua.org/manual/](https://www.lua.org/manual/)
- CSV पर और पढ़ाई के लिए: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Lua में डेटा हैंडलिंग के अन्य तरीके: [https://www.tutorialspoint.com/lua/lua_file_io.htm](https://www.tutorialspoint.com/lua/lua_file_io.htm)
