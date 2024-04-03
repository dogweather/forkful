---
date: 2024-01-20 18:02:52.252544-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.549196-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local url = "http://example.com/data"
local user = "your_username"
local password = "your_password"

local response_body = {}

http.request{
  url = url,
  method = "GET",
  headers = {
    ["Authorization"] = "Basic " .. (mime.b64(user .. ":" .. password))
  },
  sink = ltn12.sink.table(response_body)
}

-- प्रतिक्रिया देखने के लिए प्रिंट करें
print(table.concat(response_body))
```

इस कोड से आउटपुट जैसा कुछ होगा:
```
<Response contents here>
```

## गहराई में:
HTTP अनुरोध भेजना और बेसिक ऑथेंटिकेशन 1990 के दशक से वेब एप्लिकेशन में प्रचलित है। बेसिक ऑथेंटिकेशन सरल होता है, लेकिन कम सुरक्षित भी होता है, इसलिए कभी-कभी OAuth जैसे ज्यादा सुरक्षित विकल्पों का उपयोग किया जाता है। लुआ में इसे करने के लिए सामान्यतः `socket.http` लाइब्रेरी का उपयोग होता है, और प्रत्युत्तर को संभालने के लिए `ltn12`। 

चूंकि बेसिक ऑथेंटिकेशन Base64 कोडिंग का प्रयोग करता है, हमें यह कोडिंग प्रदान करने वाली `mime` लाइब्रेरी की भी आवश्यकता होती है। सुरक्षा के लिहाज़ से, HTTPS उपयोग करना और अपनी क्रेडेंशियल्स को हार्डकोड न करना सदैव बेहतर होता है।

## देखें भी:
- LuaSec लाइब्रेरी (HTTPS सपोर्ट के लिए): https://github.com/brunoos/luasec
- HTTP और अन्य Lua नेटवर्किंग ऑपरेशंस के बारे में और जानकारी: http://w3.impa.br/~diego/software/luasocket
- MIME टाइप हैंडलिंग के लिए LuaSocket MIME लाइब्रेरी: http://w3.impa.br/~diego/software/luasocket/mime.html
- अधिक सुरक्षित OAuth ऑथेंटिकेशन मेथड: https://oauth.net/
