---
date: 2024-01-20 18:01:52.349398-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:53.065872-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
```Fish Shell
function send_http_request_with_auth
    set username 'yourUsername'
    set password 'yourPassword'
    set auth_string (echo -n "$username:$password" | base64)

    # अनुरोध भेजें
    curl -H "Authorization: Basic $auth_string" https://your.api/endpoint
end

send_http_request_with_auth
```

सैंपल आउटपुट:
```
{
  "status": "success",
  "data": ...
}
```

## गहराई से:
बेसिक प्रमाणीकरण HTTP के शुरुआती दिनों से ही है और यह एक सिंपल तरीके से यूज़रनेम और पासवर्ड को बेस-64 कोडिंग के द्वारा एनकोड करके भेजता है। यद्यपि यह न कि एकदम सिक्योर होता है, लेकिन डेवलपमेंट और सिंपल एप्लीकेशन्स के लिए अक्सर पर्याप्त होता है। आजकल एडवांस प्राइवेसी टूल्स जैसे OAuth और API कीज़ का भी उपयोग होता है। फिश शैल में `base64` और `curl` जैसे टूल्स के संयोजन से यह काम सरल हो जाता है।

## देखें भी:
- cURL की आधिकारिक वेबसाइट: [https://curl.haxx.se/](https://curl.haxx.se/)
- Fish Shell का डॉक्युमेंटेशन: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- HTTP बेसिक प्रमाणीकरण विवरण: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- एडवांस प्रमाणीकरण विधियां: OAuth और API कीज: [https://oauth.net/](https://oauth.net/)
