---
title:                "HTTP अनुरोध भेजना"
html_title:           "Fish Shell: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एक HTTP अनुरोध भेजने में लोगों के इष्टतम 2 सेकंड से कम का समय लगता है। 

## कैसे

उदाहरण कोड और सैंपल आउटपुट के साथ "```Fish Shell...```" कोड ब्लॉक्स में। 

```fish
# कॉमेंट लाइन का प्रयोग करें
curl https://www.example.com

# एचटीटीपी अनुरोध स्थिति को निर्धारित करें
curl -Is https://www.example.com | head -n 1

# परिणामों में केवल हैडर दिखाएं
curl -I https://www.example.com
```

### गहराई में जाएं

HTTP अनुरोध भेजने के लिए `curl` का उपयोग करना अत्यंत सरल है। यह आपको अनुरोध करने के संपूर्ण प्रक्रिया से बचाता है और आपको उत्तरों को देखने के लिए आसान तरीके से प्रदान करता है। आप ब्राउजर में एक URL दर्ज करके अनुरोध के लिए भी `curl` का उपयोग कर सकते हैं। यह अनुरोध के लिए आपको दिए गए प्रत्येक पैरामीटर को दिखाता है और आपको उनमें से चुनने का विकल्प भी देता है।

## और भी देखें

- [Curl का मैन्युअल](https://curl.se/docs/manpage.html)
- [HTTP स्टेटस कोड की सूची](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [बातचीत करें: फिश शैल](https://gitter.im/fish-shell/fish-shell)