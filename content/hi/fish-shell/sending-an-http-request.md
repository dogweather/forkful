---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# फिश शेल का उपयोग करके HTTP अनुरोध भेजना

## क्या और क्यों?

HTTP अनुरोध एक सिग्नल होता है जो हमें वेबसर्वर से जानकारी प्राप्त करने का अनुमति देता हैं इसका इस्तेमाल करने से प्रोग्रामर्स डाटा प्राप्त कर सकते हैं और वेबसर्वर की स्थिति जांच सकते हैं।

## कैसे:

```Fish Shell
# HTTP GET अनुरोध
curl -X GET 'https://jsonplaceholder.typicode.com/posts'
```

कोड का निष्पादन करने पर, आपको वेबसर्वर से डेटा प्राप्त होगा। 

## गहराई में:

HTTP GET अनुरोध, World Wide Web का एक महत्वपूर्ण हिस्सा है जो 1990 में डिज़ाइन हुवा था। यह अनुरोध वेबसर्वर से डेटा प्राप्त करने में हमारी सहायता करता है। 

Alternatives जैसे कि HTTPie और Wget भी हैं, लेकिन फिश शेल बहुत सरल और कुशल है। HTTPie के साथ आप `-f` फ्लैग का उपयोग करके फॉर्म डाटा भेज सकते हैं। 

अनुरोध को send करने में, एक सॉकेट सिरीज बनाई जाती है, तभी TCP/IP प्रोटोकॉल स्टैक को नियंत्रित किया जाता है। 

## देखें भी:

1. Fish Shell डॉक्युमेंटेशन [लिंक](https://fishshell.com/docs/current/index.html)
2. कर्ल डॉक्युमेंटेशन [लिंक](https://curl.haxx.se/docs/manpage.html)
3. HTTPie डॉक्युमेंटेशन [लिंक](https://httpie.io/docs)
4. Wget डॉक्युमेंटेशन [लिंक](https://www.gnu.org/software/wget/manual/wget.html)