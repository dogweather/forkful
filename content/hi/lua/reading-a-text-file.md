---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
पाठ फ़ाइल पढ़ना, कंप्यूटर को फ़ाइल से डेटा पढ़ने की प्रक्रिया होती है। प्रोग्रामर्स इसे डेटा को मनिपुलेट और उपयोग करने के लिए करते हैं।

## कैसे: (How to:)
```Lua
-- एक खाका (template) पढ़ने के लिए
local file = io.open("path","r") -- 'r' पढ़ने के लिए है
local content = file:read("*all") -- फ़ाइल की सभी सामग्री पढ़ें
file:close() -- संयोग को बंद करें

print(content) -- पाठ को मुद्रित करें
```
उत्तर: "यहाँ आपकी फ़ाइल का विवरण होगा"

## गहरा डाइव (Deep Dive)

1. ऐतिहासिक संदर्भ: Lua, Lua समुदाय और कार्यकारियों द्वारा डेटा प्रक्रिया करने की क्षमता को बढ़ाने के लिए 1993 में बनाया गया था।
2. वैकल्पिक तरीके: फ़ाइल पढ़नें के कई तरीके हो सकते हैं, जैसे कि `io.lines()` जो प्रत्येक पंक्ति को पढ़ता है।
3. अंदर की बात: Lua में पाठ फाइलों को पढ़ने के लिए `io` लाइब्रेरी का उपयोग किया जाता है, जो इनपुट और आउटपुट को हैंडल करती है।

## यह भी देखें (See Also)
1. [Lua शास्त्र पुस्तकालय (Lua IO Library Documentation)](https://www.lua.org/manual/5.3/manual.html#6.8)
2. [Lua में फ़ाइलों के साथ काम करना (Working with Files in Lua)](https://www.tutorialspoint.com/lua/lua_file_io.htm)