---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP request (हाइपर टेक्स्ट ट्रांसफर प्रोटोकॉल अनुरोध) कैसे और क्यों भेजें, इसे समझना बहुत महत्वपूर्ण है। यह एक सिग्नल भेजता है जो किसी सर्वर से जानकारी लेना चाहता है। प्रोग्रामर्स इसे वेब डेटा क्षेत्रीयकरण, ऐपीआई एक्सेक्यूशन और रिमोट सर्वर एक्सेक्यूशन के लिए उपयोग करते हैं।

## कैसे करें:
Lua में HTTP request भेजने के लिए `lua-http` पैकेज का उपयोग होता है। नीचे एक उदाहरण दिया गया है:

```Lua
local http_request = require "http.request"
local _, stream = assert(http_request.new_from_uri("http://example.com"):go())
local body = assert(stream:get_body_as_string())
print(body)
```

इस कोड स्निपेट का उद्देश्य "http://example.com" पर HTTP request भेजना है और सर्वर का प्रतिसाद प्रिंट करना है।

## गहराई के संग:
HTTP अनुरोधों का इस्तेमाल 90 के दशक से हो रहा है, जब इंटरनेट नई-नई शुरू हुई थी। Lua में, इसे `lua-http` द्वारा लागू किया गया है, जो सॉकेट, कोरूटाइन्स और HTTP/1 और HTTP/2 सपोर्ट ला सकता है। `lua-http` के विकल्प में `luasocket http` शामिल है, यदि आपको आवश्यकता होती है तो।

## यह भी देखें:
अधिक जानकारी के लिए, नीचे दिए गए संसाधनों पर जाएं:
- Lua-http प्रलेखन: http://daurnimator.github.io/lua-http/
- Luasocket प्रलेखन: http://w3.impa.br/~diego/software/luasocket/http.html
- HTTP विकी पन्ना: https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol