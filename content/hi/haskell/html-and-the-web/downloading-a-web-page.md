---
date: 2024-01-20 17:44:14.436731-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) Haskell\
  \ \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\
  \u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `http-client` \u0914\u0930 `http-client-tls` \u092A\u0948\u0915\u0947\u091C\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\
  \u093E \u0939\u0948\u0964."
lastmod: '2024-04-05T22:38:53.311843-06:00'
model: gpt-4-1106-preview
summary: ") Haskell \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\
  \u094B \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `http-client` \u0914\u0930 `http-client-tls` \u092A\u0948\
  \u0915\u0947\u091C \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## कैसे करें? (How to:)
Haskell में वेब पेज को डाउनलोड करने के लिए `http-client` और `http-client-tls` पैकेज का इस्तेमाल होता है।

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://www.example.com"
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ show (responseStatus response)
    print $ responseBody response
```

यदि आप `http://www.example.com` से HTML सामग्री डाउनलोड करते हैं, आपको निम्न तरह से आउटपुट मिलेगा:

```
The status code was: 200
"<html>...</html>"
```

## गहन जानकारी (Deep Dive)
Haskell के पुराने दिनों में, `HTTP` पैकेज का उपयोग आम था, पर आधुनिक `http-client` अधिक लचीलेपन और TLS (ट्रांसपोर्ट लेयर सिक्योरिटी) सपोर्ट के साथ आता है। अल्टरनेटिव्स में `wreq` और `req` जैसे हाई-लेवल वेब क्लाइंट लाइब्रेरीज शामिल हैं। डाउनलोडिंग प्रक्रिया में, HTTP रिक्वेस्ट को बनाना, सर्वर से कनेक्ट करना, रिस्पॉन्स प्राप्त करना, और डेटा पढ़ना शामिल होता है। यह सभी कार्य `http-client` लाइब्रेरी द्वारा आसानी से किए जा सकते हैं।

## सम्बंधित स्रोत (See Also)
- Haskell `http-client` पैकेज: [Hackage](https://hackage.haskell.org/package/http-client)
- `http-client-tls` पैकेज: [Hackage](https://hackage.haskell.org/package/http-client-tls)
- वेब स्क्रैपिंग के लिए `hxt`: [Hackage](https://hackage.haskell.org/package/hxt)
- `wreq`: [Hackage](https://hackage.haskell.org/package/wreq)
- `req`: [Hackage](https://hackage.haskell.org/package/req)
