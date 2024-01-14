---
title:                "Haskell: हटएपी प्रेषण भेजना"
simple_title:         "हटएपी प्रेषण भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

इस ब्लॉग पोस्ट में हम बात करेंगे हैस्केल में HTTP रिक्वेस्ट भेजने के बारे में। हम अपने प्रोजेक्ट में डेटा को अन्य सर्वर से लेने या भेजने के लिए इस काम का इस्तेमाल करते हैं।

## क्यों

HTTP रिक्वेस्ट भेजने का सबसे आसान तरीका है सर्वर से डेटा को लेना या भेजना। यह आपको स्थानान्तरणीय संबंध के बिना डेटा को सुरक्षित तरीके से साझा करने की अनुमति देता है।

## कैसे करें

आइए एक उदाहरण के साथ देखें कि हम हैस्केल में एक GET रिक्वेस्ट कैसे भेज सकते हैं। सबसे पहले, हम आवश्यक पैकेजों को लोड करेंगे।

```Haskell
import Network.HTTP
import Text.HTML.TagSoup (parseTags, fromAttrib)
```

उपरोक्त कोड सारणी के साथ Network.HTTP पैकेज हैस्केल में एचटीटीपी रिक्वेस्ट भेजने का एपीआई प्रदान करता है। Text.HTML.TagSoup पैकेज सामग्री को एचटीएमएल पैमाने पर पार्स करने के लिए उपयोग किया जाता है।

अब हम किसी वेबसाइट का यूआरएल और हैडर्स के साथ एक GET रिक्वेस्ट बनाएंगे।

```Haskell
url = "https://www.example.com"
headers = [("User-Agent", "Haskell-Blog"), ("Accept", "text/html")]
request = getRequest url headers
```

ऊपर, हम एक GET रिक्वेस्ट बनाया है जो मुख्य यूआरएल और हैडर्स सहित है। अब, हम उस रिक्वेस्ट को भेजने और सर्वर से डेटा प्राप्त करने के लिए नेटवर्क एक्शन का उपयोग करेंगे।

```Haskell
response <- simpleHTTP request
htmlBody <- getResponseBody response
```

ऊपर वाले कोड में, हम रिक्वेस्ट भेजते हैं और उसके बाद सर