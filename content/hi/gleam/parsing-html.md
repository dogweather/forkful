---
title:                "Gleam: HTML पार्सिंग"
simple_title:         "HTML पार्सिंग"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

HTML पार्सिंग में शामिल होने के दौरान, आप वेबसाइटों से डेटा को स्क्रैप कर सकते हैं। यह आपको डेटा का अनुकरण करने और उसे अपनी अनुकूलता के अनुसार संरचित करने की अनुमति देता है।

## कैसे करें

जैसा कि हम सभी जानते हैं, वेबसाइटों में डेटा हमारे अनुकूलता के लिए ऊपर से आता है। ग्लेम में, हम केवल HTML को पार्स करके उसे डेटा स्ट्रक्चर में परिवर्तित करने के लिए थोड़ी सी कोड लिख सकते हैं। इसके लिए `html.parse` फंक्शन का उपयोग करें:

```
Gleam> html.parse("<h1>Hello World</h1>")
ᚕOk(ᚐ
    [
        {
            tag = "h1",
            children = [
                {
                    tag = (),
                    text = "Hello World",
                    attributes = (),
                    children = [()],
                },
            ],
            attributes = (),
            children = [
                {
                    tag = (),
                    text = "Hello World",
                    attributes = (),
                    children = [()],
                },
            ],
        },
    ],
)
```

आप सामान्य ग्राहकों को बाहर सिर्फ `<h1>Hello World</h1>` देख सकते हैं, लेकिन `html.parse` फंक्शन इसे स्पष्ट रूप से स्ट्रक्चर में डेटा के साथ प्रस्तुत करता है।

## गहराई में जायें

HTML पार्स करना थोड़ा सामान्य है, लेकिन भाषा की सामग्री को समझने और उसे स्ट्रक्चर में लाने में प्रतिभागियों को गहराई में जाने की आवश्यकता हो सकती है। इसलिए, आप #Gleam-HTML [प्रोजेक्ट में](https://github.com/gleam-lang/gleam_html) और [विशेष प्रोजेक्ट सिद्धांतों में](https://gleam.run/#org812c478) गहराई में प्रवेश कर सकते हैं।

## देखें भी

- [ग्लेम डॉक्यूमेंटेशन](https://gleam.run/)
- [ग्लेम-HTML प्रोजेक्ट](https://github.com/gleam-lang/gleam_html)