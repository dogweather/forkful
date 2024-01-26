---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:01:52.349398-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

बुनियादी प्रमाणीकरण के साथ HTTP अनुरोध भेजना यानी कि वेब सर्वर को यूज़रनेम और पासवर्ड संलग्न करके डेटा मांगना। प्रोग्रामर्स इसे तब उपयोग करते हैं जब सिक्योर एक्सेस की जरूरत होती है।

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
