---
title:                "Elm: बेसिक प्रमाणीकरण सहित एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण सहित एक http अनुरोध भेजना"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने में बुनियादी प्रमाणीकरण के साथ क्यों शामिल होना?

आमतौर पर, एप्लिकेशन डेवलपर एपीआई से डेटा प्राप्त करने के लिए एचटीटीपी अनुरोध भेजते हैं। डेटा को सुरक्षित रूप से भेजने के लिए, अधिकांश एपीआई बेसिक प्रमाणीकरण का उपयोग करती हैं। बेसिक प्रमाणीकरण एक आसान और आम में प्रयुक्त प्रमाणीकरण होता है जो अनुरोध भेजने वाले उपयोगकर्ता के उपयोगकर्ता नाम और पासवर्ड को रिक्त परिवर्तन से भेजता है। इससे एप्प प्रमाणीकरण को वापस पाने में सहायता मिलती है और आसानी से सुरक्षित डेटा प्राप्त करने में मदद करता है।

## कैसे करें

एल्म में एचटीटीपी अनुरोध भेजना बेहद आसान है। निम्नलिखित कोड उदाहरण दिखाता है कि कैसे एचटीटीपी अनुरोध के साथ बेसिक प्रमाणीकरण को ईम्पलीमेंट किया जाता है:

```Elm
import Http
import BasicAuth

-- अनुरोध भेजने के लिए एचटीटीपी एपर
sendRequest : Http.Request BasicAuth.Auth
sendRequest =
  let
    -- डेटा को प्राप्त करने के लिए उपयोगकर्ता नाम और पासवर्ड
    username = "example_user"
    password = "example_password"
    -- एचटीटीपी अनुरोध
    request =
      Http.request
        { method = "GET"
        , headers = []
        , url = "https://exampleapi.com/data"
        , body = Http.emptyBody
        }
    -- बेसिक प्रमाणीकरण का उपयोग करने के लिए एचटीटीपी एपर
    basicAuth = BasicAuth.encode username password
  in
    -- बेसिक प्रमाणीकरण के साथ अनुरोध भेजने के लिए एचटीटीपी
    Http