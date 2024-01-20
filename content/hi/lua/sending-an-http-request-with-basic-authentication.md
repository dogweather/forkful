---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP request और basic authentication एक प्रकार की कोडिंग क्रिया हैं जिसका उपयोग programmers भुनावटियों, सर्वर और डेटा की अवधारणा के लिए करते हैं। यह प्रक्रिया निजी डेटा की सुरक्षा में सहयोग देती है और ग़ैर-औरोरित सभी संचारों को रोकती है।

## कैसे करें:

```Lua
-- LuaSocket और LuaSec लाइब्रेरिज़ की स्थापना 
local http = require("socket.http")
local https = require("ssl.https")
local ltn12 = require("ltn12")

-- यूज़रनेम और पासवर्ड
local user = 'your_username'
local password = 'your_password'

-- आवेदन का पाठ
local auth = 'Basic ' .. (mime.b64(user .. ':' .. password))

-- एचटीटीपी अनुरोध भेजना
local response_body, status, response_headers = http.request
{
    url = "http://example.com",
    method = "GET",
    headers =
    {
        Authorization = auth,
    },
}

-- प्रतिसाद दिखाना
print(response_body)
```

## गहन विवेचना

एचटीटीपी basic authentication को HTTP/1.0 विनिर्देश में पहली बार पेश किया गया था। यह एक साधारण और ईमानदार तरीका प्रमाणीकरण करने का है जो यूज़र नाम और पासवर्ड को Base64 कोडिंग द्वारा कोड किया जाता है।

इसके विकल्प में Digest Authentication, NTLM Authentication, और Form-Based Authentication आते हैं जिन्हें भी तथा स्थानीय आवश्यकताओं के आधार पर इस्तेमाल किया जा सकता है।

स्थापना में LuaSocket और LuaSec (HTTPS के लिए) लाइब्रेरिज़ का इस्तेमाल हुआ है। इसमें `http.request` का इस्तेमाल करके URL, विधि, और हेडर को एक तालिका द्वारा पारित किया जाता है।

## भी देखें

- [HTTP Basic Authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [LuaSocket HTTP reference](http://w3.impa.br/~diego/software/luasocket/http.html)
- [LuaSec reference](https://github.com/brunoos/luasec/wiki)