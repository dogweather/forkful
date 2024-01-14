---
title:                "Haskell: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों

HTTP अनुरोध भेजने में आमतौर पर प्रयुक्तियों को बेसिक प्रमाणीकरण के साथ अभिगमित किया जाता है। यह सुरक्षा स्तरों को संभावित अनुरोधकर्ताओं से संभालने के विषय में थोड़ा प्रभावकारी होता है और उन्हें सेंट्रल जनरेशन मेथड के आदेशों और उत्पादों को अनुप्रयोगित करने से बचाता है।

# कैसे करें

कुछ भीनताहूं हेकेल कोडिंग का एक उदाहरण दिया जाएगा, जो त्रुटि निम्न होगी:

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (defaultManagerSettings)

main :: IO ()
main =
    do
        request <- parseRequest "GET http://httpbin.org/basic-auth/user/passwd"
        response <- httpBS request `digest` "user" "password"
        let code = statusCode $ getResponseStatusCode response
        BS.putStrLn $ BS.pack ("Response status: " ++ show code)
```

इस उदाहरण का आउटपुट निम्न होगा:

`Response status: 200`


# गहराई में सम्झे

HTTP अनुरोध भेजने के लिए बेसिक प्रमाणीकरण का उपयोग करने के लिए, हमें सिद्धांत को समझना होगा। बेसिक प्रमाणीकरण के साथ, उपयोगकर्ता नाम और पासवर्ड को सर्वर तक इन अनुरोधों में एन्कोड किया जाता है। फिर, सर्वर अनुरोधकर्ता के पास उपयोगकर्ता नाम और पासवर्ड को सत्यापित कर रहा है, और यदि सफल होता है, तो वह अनुरोध को संक्रिय रूप से प्रचालित करता है। हमने भी `Network.HTTP.Simple` मॉड्यूल के साथ कैसे काम करने की जानकारी दी, जो अनुरोध भेजने के लिए सरल साधन प्रदान करता है।

# देखें भी

- [Haskell Documentation on HTTP Requests](https://www.haskell.org/documentation/)
- [HTTP Client Library](https://www.stackage.org/package/http-client)
- [Simple Authentication with HTTP](https://en.wikipedia.org/wiki/Basic_access_authentication)