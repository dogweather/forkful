---
title:                "XML के साथ काम करना"
aliases:
- /hi/lua/working-with-xml/
date:                  2024-01-26T04:34:49.786757-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना कोड का उपयोग करके XML दस्तावेजों को पार्स करने और उन्हें संशोधित करने में शामिल है। प्रोग्रामर इसे डेटा को पढ़ने, लिखने और संशोधित करने के लिए करते हैं, जो एक संरचित, पोर्टेबल प्रारूप में होता है जो डेटा विनिमय और भंडारण के लिए व्यापक रूप से इस्तेमाल होता है।

## कैसे करें:
Lua में मूल XML पार्सिंग शामिल नहीं है, लेकिन LuaXML और xml2lua जैसे पुस्तकालय हैं जो काम को पूरा करते हैं। यहाँ xml2lua के साथ XML पार्स करने की एक त्वरित झलक है:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- आउटपुट: 123
print(handler.root.book[1])        -- आउटपुट: Programming in Lua
```

XML लिखने के लिए, यहाँ LuaXML का उपयोग करके एक मिनी उदाहरण है:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- आउटपुट: <root><book id="123">Programming in Lua</book></root>
```

## गहन चर्चा
XML, जिसका पूरा नाम Extensible Markup Language है, 90 के दशक के मध्य से डेटा प्रतिनिधित्व और विनिमय में एक मानक रहा है। यह डेटा को संरचना प्रदान करता है और यह मानव-पठनीय और मशीन-पार्स करने योग्य दोनों है।

जहां JSON और YAML अब उनकी सादगी के कारण पसंदीदा हैं, XML कई एंटरप्राइज और पुरानी प्रणालियों में प्रचलित रहता है। Lua में मूल XML हैंडलिंग बिल्ट-इन नहीं है क्योंकि Lua को छोटा और मॉड्यूल के माध्यम से विस्तृत करने के लिए डिज़ाइन किया गया है।

Lua के लिए XML पुस्तकालय, जैसे LuaXML, xml2lua, और अन्य, इस गैप को पाटते हैं। LuaXML एक हल्का XML रीडर और राइटर प्रदान करता है, जबकि xml2lua एक घटना-चालित दृष्टिकोण का उपयोग करता है जो SAX पार्सरों के समान है। ये पुस्तकालय आमतौर पर पोर्टेबिलिटी के लिए शुद्ध Lua में लागू किए जाते हैं, जबकि कुछ प्रदर्शन के लिए C पर निर्भर हो सकते हैं।

प्रदर्शन और मेमोरी उपयोग के संदर्भ में, Lua के XML पुस्तकालय मूल समर्थन वाली भाषाओं के पुस्तकालयों की तुलना में उतने तेज़ नहीं हो सकते हैं। हालांकि, Lua में अधिकांश उपयोग-मामलों के लिए, विशेष रूप से गेम डेवलपमेंट या एम्बेडेड सिस्टम के लिए स्क्रिप्टिंग में, ये पुस्तकालय बिना प्रणाली को ओवरलोड किए ठीक काम करते हैं।

## और भी देखें
- LuaXML GitHub पर: https://github.com/LuaDist/luaxml
- xml2lua GitHub पर: https://github.com/manoelcampos/xml2lua
- Lua.org की पुस्तकालयों की सूची: https://lua-users.org/wiki/LibrariesAndBindings
