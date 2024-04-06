---
date: 2024-01-20 18:00:49.229065-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Lua \u092E\
  \u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, 'lua-http' \u092F\u093E 'socket.http' \u091C\u0948\
  \u0938\u0947 \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\u091A\
  \u0947 'lua-http' \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0938\u093E\u0927\u093E\u0930\u0923 GET\u2026"
lastmod: '2024-04-05T21:53:54.522348-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Lua \u092E\u0947\u0902\
  \ HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F, 'lua-http' \u092F\u093E 'socket.http' \u091C\u0948\u0938\
  \u0947 \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\u091A\u0947\
  \ 'lua-http' \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u0938\u093E\u0927\u093E\u0930\u0923 GET \u0905\u0928\u0941\u0930\u094B\u0927\
  \ \u0915\u093E \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें)
Lua में HTTP अनुरोध भेजने के लिए, 'lua-http' या 'socket.http' जैसे मॉड्यूल का उपयोग होता है। नीचे 'lua-http' का उपयोग करके साधारण GET अनुरोध का उदाहरण है:

```Lua
local http = require('http')

local function fetchUrl(url)
    local response, err = http.request("GET", url)
    if not response then
        print("HTTP अनुरोध में त्रुटि:", err)
        return
    end
    print(response:get_body_as_string())
end

fetchUrl("http://httpbin.org/get")
```

यह कोड एक वेब पेज से कंटेंट को प्रिंट करेगा। अगर त्रुटि आती है, तो वह भी प्रिंट होगी।

## Deep Dive (गहराई से जानकारी)
HTTP (HyperText Transfer Protocol) इंटरनेट पर डेटा संचार का मुख्य माध्यम है। इसकी शुरुआत 1989 में हुई थी। Lua में, हमे सरल HTTP अनुरोध के लिए 'lua-http' या 'LuaSocket' जैसे तीसरे पक्ष के लाइब्रेरीज का सहारा लेना पड़ता है। 'lua-http' अधिक मॉडर्न और फुल-फीचर्ड है, जबकि 'LuaSocket' पुराना पर मजबूत है। 

इन दोनों का विकल्प REST API के संपर्क में आने के लिए, संदेश स्वरूप (JSON या XML) में कन्वर्ट करना, और हेडर्स, पैरामीटर्स, और अथेंटिकेशन जैसे बारीकियाँ संभालना अहम हैं।

## See Also (इसे भी देखें)
- Lua HTTP documentation: [https://github.com/daurnimator/lua-http](https://github.com/daurnimator/lua-http)
- HTTP बिन (टेस्ट HTTP अनुरोध): [http://httpbin.org/](http://httpbin.org/)
