---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# एल्एम में बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना 

## क्या और क्यों?
HTTP अनुरोध के माध्यम से, हम वेबसर्वर कोन्नेक्ट करते हैं और डेटा को या तो प्राप्त करते हैं या भेजते हैं। बेसिक प्रमाणीकरण का उपयोग करके, हम हमारे अनुरोध की सुरक्षा सुनिश्चित करते हैं, और यह सत्यापित करतें हैं कि केवल अधिकृत यूजर्स ही डेटा का उपयोग कर सकते हैं।

## कैसे करें: 
एल्एम का उपयोग करते हुए, हम ऐसा कैसे कर सकते हैं, अब देखते हैं ।

```Elm
import Http
import Http.Headers as Headers

basicAuth : String -> String -> Http.Header
basicAuth username password =
    let
        token =
            Basics.toBase64 (username ++ ":" ++ password)
    in
        Headers.authorization ("Basic " ++ token)

-- Example usage
myRequest : Http.Request String
myRequest =
    Http.request
        { method = "GET"
        , headers = [ basicAuth "my-username" "my-password" ]
        , url = "http://my-server.com"
        , body = Http.emptyBody
        , expect = Http.expectString (Result.Ok >> Ok)
        , timeout = Nothing
        , tracker = Nothing
        }
```

## गहरी छानबीन :
बेसिक प्रमाणीकरण भेजते समय, यूज़रनेम और पासवर्ड को Base64 में कोड करते हैं। यह पद्धति 1990 के दशक में HTTP/1.0 के साथ विकसित की गई थी। इसके विकल्पों में डाइजेस्ट एक्सेस प्रमाणीकरण और OAuth हैं, जो एक अधिक सुरक्षित तरीके से यूज़र का प्रमाणीकरण सत्यापित करते हैं। यदि आपको अधिक सुरक्षा की आवश्यकता है, तो आपके पास इनका उपयोग करना विकल्प है।

## और देखें :
1. [Elm's HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
2. [Basics in Elm](https://elm-lang.org/docs/from-javascript)
3. [Basic Authorization on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)