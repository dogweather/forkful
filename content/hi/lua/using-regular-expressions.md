---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
नियमित अभिव्यक्तियाँ (Regular expressions) पैटर्न मिलान के लिए इस्तेमाल होती हैं। प्रोग्रामर्स इसका उपयोग टेक्स्ट सर्चिंग, वैधता की जांच, और मोडिफिकेशन के लिए करते हैं।

## कैसे करें:
```Lua
local text = "आज का मौसम अच्छा है।"
local pattern = "मौसम"

-- साधारण पैटर्न मिलान
if string.match(text, pattern) then
    print("पैटर्न मिल गया!")
else
    print("पैटर्न नहीं मिला।")
end
```
आउटपुट: पैटर्न मिल गया!

```Lua
local extract = string.gsub(text, "(%a+)", function(word)
    return "[" .. word .. "]"
end)
print(extract)
```
आउटपुट: [आज] [का] [मौसम] [अच्छा] [है]।

## गहराई से जानकारी:
नियमित अभिव्यक्तियाँ 1950 के दशक से हैं। इनके विकल्प के रूप में स्ट्रिंग संबंधी फंक्शन्स और पार्सर्स होते हैं। Lua में, `string` लाइब्रेरी मिलान, खोज, और प्रतिस्थापन के लिए पैटर्न मैचिंग प्रदान करती है जो POSIX या Perl के रेगुलर एक्सप्रेशन्स से थोड़ी अलग होती है।

## इसे भी देखें:
- [Lua 5.4 रेफरेंस मैनुअल](https://www.lua.org/manual/5.4/)
- [Pattern Matching in Lua - Stack Overflow](https://stackoverflow.com/questions/tagged/lua+pattern-matching)
