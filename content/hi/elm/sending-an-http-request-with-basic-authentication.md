---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:02:22.537148-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request-with-basic-authentication.md"
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