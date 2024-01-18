---
title:                "HTTP अनुरोध भेजना"
html_title:           "Lua: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP request भेजना क्या है, और क्यों करते हैं, इसके बारे में ये दो-तीन वाक्य बताते हैं। वे इसे वेब डेवलपमेंट में डेटा को पास करने का सबसे प्रमुख तरीका बताते हैं।

## कैसे करें:
जब आप HTTP request भेजते हैं, आपका ब्राउज़र डेटा को एक वेब सर्वर से लेकर आता है और उसे डिस्प्ले करता है। बहुत सारे वेब साइट में, आप नोटिस कर सकते हैं कि ब्राउज़र के URL बार में कुछ डाटा होता है, जो तारीख, समय, स्थान या उपयोगकर्ता की पहचान को प्रदर्शित करता है। बहुत सारे वेब साइटों के पीछे ये डाटा HTTP request के जरिए होता है।

आपको एक HTTP request भेजने के लिए कुछ simple steps फॉलो करने होंगे:
```
Lua

-- 1. अपने कोड में http module जोड़ें:
local http = require("socket.http")

-- 2. आपकी रिकवेस्ट के लिए एक URL तय करें:
local url = "https://example.com"

-- 3. http.request() का प्रयोग करें और उसका response स्टोर करें:
local response = http.request(url)

-- 4. जब आप चाहें, response को प्रिंट करें:
print(response)
```

ये आपको आपकी रिक्वेस्ट का response दिखाएगा, जो आमतौर पर HTML, XML या JSON फॉर्मेट में होता है।

## गहराई जाँच:
HTTP request का इतिहास बहुत पुराना है, और ये web डेवलपमेंट के शुरुआती दिनों से ही उपयोग में है। ये किसी भी वेब डेवलपर के लिए आवश्यक है, क्योंकि ये ब्राउज़र और सर्वर के बीच कम्यूनिकेशन बनाता है। अन्य विकल्पों में, आप भी HTTP request को करने के लिए cURL, Postman जैसे उपकरण का उपयोग कर सकते हैं।

HTTP request को बहुत ही सरल और स्ट्रेटफ़ारड तरीके से implement किया जाता है। प्रमुख भाषाओं में से एक Lua भी एक http library प्रदान करती है जो HTTP request को भेजने के लिए काम करती है। इस लाइब्रेरी को समझने के लिए आपको Lua में network programming की भी जानकारी होनी चाहिए।

## इससे जुड़ा भी देखें:
- [luahttp](https://github.com/hishamhm/luahttp) - Lua के लिए एक साधारण HTTP multiplexer
- [HTTP रिक्वेस्ट के बारे में सिखना](https://www.tutorialspoint.com/http/http_requests.htm) - एक HTTP रिक्वेस्ट कंप्यूटर नेटवर्किंग में कैसे काम करता है का एक प्रच्यून ट्युटोरियल