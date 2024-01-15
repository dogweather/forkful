---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों
डिबग आउटपुट प्रिंट करने में क्यों दिलचस्पी हो सकती है। 

## कैसे करें 
वर्तमान संस्करण में Gleam प्रोग्रामिंग के लिए डिबग आउटपुट प्रिंट कैसे करें:

```
Gleam.debug("Hello World!")
```

इस `Gleam.debug` फंक्शन के उपयोग से हम कोड के अंदर संदेश दिखा सकते हैं। यह हमें अपने कोड को पहचानने और मॉड्यूल के विभिन्न हिस्सों में क्या हो रहा है इसकी जानकारी देता है। 

```
Gleam.debug("My favorite number is" ++ 3)
```

इस उदाहरण में, हम अपने कोड में मौजूद संख्या का सीधा मूल्य प्रिंट कर सकते हैं। 

## गहराई में
डिबग आउटपुट प्रिंट करने के साथ हम अपने कोड के अंदर कीमतों को देखने के अलावा, विभिन्न लेवलों पर भी संदेश प्रिंट कर सकते हैं। इसके लिए, हम `Gleam.debug` फंक्शन के तीन पैरामीटर पास कर सकते हैं: संदेश, डिबग लेवल और स्टैक ट्रेस ऑप्शन। 

```
Gleam.debug("Hello World!", 2, true)
```

यह सेटिंग हमारे संदेश को क़ेवल दूसरे और उसके उपरों के लेवलों पर ही प्रिंट करेगी और साथ ही हमें कोड का स्टैक ट्रेस भी मिलेगा। 

## देखें भी
- [Gleam डिबगिंग डॉक्यूमेंटेशन](https://gleam.run/documentation/debugging)
- [डिबगिंग सीखने का खेल](https://www.codingame.com/playgrounds/8120/introduction-to-debugging/intro) 
- [Gleam ऑफिशियल वेबसाइट](https://gleam.run)