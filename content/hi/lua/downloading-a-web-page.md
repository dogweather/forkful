---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक वेब पेज को डाउनलोड करना का अर्थ होता है उसे इंटरनेट से अपने कंप्यूटर पर लाना। प्रोग्रामर्स इसे डाटा खोजने, उसे विश्लेषित करने और अन्य वेबसाइटों के साथ अपने एप्लीकेशन को इंटरैक्ट करने के लिए करते हैं। 

## कैसे करें:

Lua में आपको वेब पेज डाउनलोड करने के लिए `luasocket` और `luasec` लाइब्रेरियां की आवश्यकता होगी। यहां एक उदाहरण है:

```Lua
http = require("socket.http")
https = require("ssl.https")

-- HTTP
local page = http.request("http://example.com")
-- HTTPS
local page = https.request("https://example.com")

print(page)
```
यहां `http.request` और `https.request` वेब पेज को डाउनलोड करते हैं और उसकी सामग्री को लौटा देते हैं। 'print(page)' सारी सामग्री को प्रिंट करेगा।

## गहराई में:

Lua में वेब पेज को डाउनलोड करना बहुत साधारण है, लेकिन इसके पीछे कुछ इतिहास है। Lua  भाषा को 1993 में ब्राजील में लैब्रेटरी के लिए निर्माण किया गया था, जिसमें इंटरैक्टिव टेलीविजन के लिए एप्लिकेशन बनाने का आवश्यकता थी। बाद में इसे वेब डेवलपमेंट, एम्बेडेड सिस्टम, और गेम डेवलपमेंट के लिए भी इस्तेमाल किया गया। 

कई अन्य भाषाओं में भी सही URL से डाटा को प्राप्त करने का समर्थन होता है, जैसे की Python (जिसमें `requests` लाइब्रेरी है), JavaScript (जिसमें `fetch` API है), और अन्य।

## अन्य संसाधन:

1. [Lua विकि बुक](https://en.wikibooks.org/wiki/Lua_Programming)
2. [LuaSocket डॉक्स](https://github.com/Lua-cURL/Lua-cURLv3)
3. [LuaSec डॉक्स](https://github.com/brunoos/luasec) 

इन स्रोतों से आप कोडेक्स, एपीआई, और अन्य कार्यों जैसे कि कैसे हेडर्स सेट करें, कैसे GET और POST अनुरोध भेजें, और बहुत कुछ सीख सकते हैं।