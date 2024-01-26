---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:49.229065-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना यानी वेब सर्वर से डेटा मांगना। प्रोग्रामर इसे वेब एपीआई के साथ बातचीत, वेबसाइट्स से डेटा हासिल करने, और वेब सेवाओं को कमांड भेजने के लिए करते हैं।

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
