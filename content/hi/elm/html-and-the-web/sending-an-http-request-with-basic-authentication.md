---
date: 2024-01-20 18:02:22.537148-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP Basic\
  \ Authentication \u0935\u0947\u092C \u0915\u0940 \u0936\u0941\u0930\u0941\u0906\u0924\
  \ \u0938\u0947 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\u0964 \u092F\u0939 \u0938\
  \u0930\u0932 \u0939\u0948 \u0932\u0947\u0915\u093F\u0928 \u0939\u092E\u0947\u0902\
  \ HTTPS \u0915\u0947 \u0938\u093E\u0925 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0928\u093E \u091A\u093E\u0939\u093F\u090F \u0935\u0930\
  \u0928\u093E \u0915\u094D\u0930\u0947\u0921\u0947\u0902\u0936\u093F\u092F\u0932\u094D\
  \u0938 \u0906\u0938\u093E\u0928\u0940\u2026"
lastmod: '2024-04-05T22:51:06.875570-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP Basic Authentication\
  \ \u0935\u0947\u092C \u0915\u0940 \u0936\u0941\u0930\u0941\u0906\u0924 \u0938\u0947\
  \ \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\u0964 \u092F\u0939 \u0938\u0930\u0932\
  \ \u0939\u0948 \u0932\u0947\u0915\u093F\u0928 \u0939\u092E\u0947\u0902 HTTPS \u0915\
  \u0947 \u0938\u093E\u0925 \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E \u091A\u093E\u0939\u093F\u090F \u0935\u0930\u0928\u093E\
  \ \u0915\u094D\u0930\u0947\u0921\u0947\u0902\u0936\u093F\u092F\u0932\u094D\u0938\
  \ \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u091A\u094B\u0930\u0940 \u0939\u094B\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 Elm \u092E\u0947\u0902, `Http`\
  \ \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0914\u0930 `Base64` \u092A\u0948\u0915\
  \u0947\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u0906\u092A \u0907\u0938\u0947 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0907\
  \u092E\u094D\u092A\u0932\u0940\u092E\u0947\u0902\u091F \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939 \u0924\u0930\u0940\u0915\u093E\
  \ \u091C\u094D\u092F\u093E\u0926\u093E\u0924\u0930 APIs \u0915\u0947 \u0932\u093F\
  \u090F \u092A\u0930\u094D\u092F\u093E\u092A\u094D\u0924 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u091C\u091F\u093F\
  \u0932 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u091C\u0948\
  \u0938\u0947 \u0915\u093F OAuth \u0915\u0947 \u0932\u093F\u090F \u0905\u0932\u0917\
  \ \u0924\u0930\u0940\u0915\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0907\u092E\u094D\u092A\u0932\u0940\u092E\u0947\u0902\u091F\u0947\u0936\u0928\
  \ \u092E\u0947\u0902, \u0915\u094D\u0930\u0947\u0921\u0947\u0902\u0936\u093F\u092F\
  \u0932\u094D\u0938 \u0915\u094B \u092C\u0947\u0938-\u096C\u096A \u092E\u0947\u0902\
  \ \u0915\u094B\u0921 \u0915\u0930\u0928\u093E \u0914\u0930 \u0939\u0947\u0921\u0930\
  \ \u092E\u0947\u0902 `Basic` \u0938\u094D\u0915\u0940\u092E \u0915\u0947 \u0938\u093E\
  \u0925 \u091C\u094B\u0921\u093C\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948\u0964 Elm \u092E\u0947\u0902 \u0906\u092A `Http.header` \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0906\u0938\u093E\u0928\u0940\
  \ \u0938\u0947 \u0915\u0938\u094D\u091F\u092E \u0939\u0947\u0921\u0930\u094D\u0938\
  \ \u092C\u0928\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

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
