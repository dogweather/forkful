---
date: 2024-01-20 18:02:22.537148-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u091C\u093F\u0938\u092E\u0947\
  \u0902 \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\
  \u0915\u0930\u0923 \u0936\u093E\u092E\u093F\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948 \u090F\u0915 \u0938\u0930\u0932 \u0935\u093F\u0927\u093F \u0939\u0948 \u091C\
  \u0939\u093E\u0902 \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930 \u092A\
  \u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u094B \u092C\u0947\u0938-\u096C\u096A\
  \ \u092E\u0947\u0902 \u0915\u094B\u0921 \u0915\u0930\u0915\u0947 \u0939\u0947\u0921\
  \u0930 \u092E\u0947\u0902 \u092D\u0947\u091C\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0915\u093E\u2026"
lastmod: 2024-02-19 22:05:11.186935
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u091C\u093F\u0938\u092E\u0947\
  \u0902 \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\
  \u0915\u0930\u0923 \u0936\u093E\u092E\u093F\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948 \u090F\u0915 \u0938\u0930\u0932 \u0935\u093F\u0927\u093F \u0939\u0948 \u091C\
  \u0939\u093E\u0902 \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930 \u092A\
  \u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u094B \u092C\u0947\u0938-\u096C\u096A\
  \ \u092E\u0947\u0902 \u0915\u094B\u0921 \u0915\u0930\u0915\u0947 \u0939\u0947\u0921\
  \u0930 \u092E\u0947\u0902 \u092D\u0947\u091C\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0915\u093E\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध जिसमें बेसिक प्रमाणीकरण शामिल होता है एक सरल विधि है जहां यूजरनेम और पासवर्ड को बेस-६४ में कोड करके हेडर में भेजते हैं। प्रोग्रामर्स इसका उपयोग उस सर्विस से जुड़ने के लिए करते हैं जिसके लिए पहचान और अधिकारीकरण आवश्यक हैं।

## How to: (कैसे करें:)

```Elm
import Http
import Base64

-- बेसिक प्रमाणीकरण हेडर बनाएं
authorizationHeader : String -> String -> Http.Header
authorizationHeader username password =
  let
    encodedCredentials = Base64.encode (username ++ ":" ++ password)
  in
  Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- HTTP GET अनुरोध भेजें
getWithBasicAuth : String -> String -> String -> Http.Request String
getWithBasicAuth url username password =
  Http.request
    { method = "GET"
    , headers = [ authorizationHeader username password ]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectStringResponse identity
    , timeout = Nothing
    , tracker = Nothing
    }

-- उपयोग का उदाहरण
sampleRequest : Http.Request String
sampleRequest =
  getWithBasicAuth "http://example.com/data" "user123" "password456"
```

## Deep Dive (गहराई से जानकारी)

HTTP Basic Authentication वेब की शुरुआत से मौजूद है। यह सरल है लेकिन हमें HTTPS के साथ इसका उपयोग करना चाहिए वरना क्रेडेंशियल्स आसानी से चोरी हो सकते हैं। Elm में, `Http` मॉड्यूल और `Base64` पैकेज का उपयोग करके आप इसे आसानी से इम्पलीमेंट कर सकते हैं। यह तरीका ज्यादातर APIs के लिए पर्याप्त होता है। हालांकि, जटिल प्रमाणीकरण जैसे कि OAuth के लिए अलग तरीके होते हैं।

इम्पलीमेंटेशन में, क्रेडेंशियल्स को बेस-६४ में कोड करना और हेडर में `Basic` स्कीम के साथ जोड़ना शामिल है। Elm में आप `Http.header` का उपयोग करके आसानी से कस्टम हेडर्स बना सकते हैं।

## See Also (देखें भी)

- [Elm Http Package Documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Base64 Elm Package](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [Elm Lang Official Guide](https://guide.elm-lang.org/)
