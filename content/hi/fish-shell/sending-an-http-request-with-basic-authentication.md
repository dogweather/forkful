---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यूँ?

HTTP अनुरोध के साथ मूल प्रमाणीकरण संचारण का प्रयोग नेटवर्क प्रोटोकॉल में किसी भी केंद्र के साथ सुरक्षित संचार करने के लिए किया जाता है। प्रोग्रामर्स इसका उपयोग करते हैं क्योंकि यह किसी भी डेटा को सुरक्षित रूप से ट्रांसफर करने का अच्छा तरीका है।

## कैसे करें:

आप निम्नलिखित कोड की सहायता से HTTP अनुरोध के साथ मूल प्रमाणीकरण कर सकते हैं:

```Fish Shell
function http_request
  curl -u username:password http://example.com
end
```

आपका उत्तर कुछ इस तरह दिखाई देगा:

```Fish Shell
< HTTP/1.1 200 OK
< Date: Mon, 01 Jan 2021 00:00:01 GMT
< Content-Type: application/json
< Content-Length: 140
...
```
## गहरी जाँच:

HTTP प्रमाणीकरण का इतिहास 1990 के ही दशक में शुरू हुआ था, जब WWW का विकास हुआ। अल्टरनेटिव्स में OAuth, JWT, Digest Access Authentication और अन्य तकनीकें शामिल हैं। HTTP प्रमाणीकरण की कार्रवाई जैसे कि डाटा को base64 एन्कोडिंग के साथ एन्कोड करना और 'Authorization' हेडर में एम्बेड करना शामिल है।

## विभिन्न स्रोत यात्रा करें:

1. [HTTP प्रमाणीकरण](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Fish के बारे में अधिक जानकारी](https://fishshell.com/docs/current/index.html)
3. [HTTP प्रोटोकॉल](https://www.w3.org/Protocols/)
4. [Basic Access Authentication](https://tools.ietf.org/html/rfc7617)
5. [CURL प्रमाणीकरण](https://curl.haxx.se/)
6. [फिश शेल कॉडिंग प्रक्रिया](https://www.youtube.com/watch?v=1leaYBZttK4)