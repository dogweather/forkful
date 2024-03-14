---
date: 2024-01-20 17:44:14.436731-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u092A\u0943\u0937\
  \u094D\u0920 \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B\
  \ \u0938\u094D\u0925\u093E\u0928\u0940\u092F \u0938\u093F\u0938\u094D\u091F\u092E\
  \ \u092A\u0930 \u0938\u0939\u0947\u091C\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\
  \u091F\u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0935\u0947\u092C\
  \ \u0938\u094D\u0915\u094D\u0930\u0948\u092A\u093F\u0902\u0917, \u092F\u093E \u0911\
  \u092B\u0932\u093E\u0907\u0928 \u092A\u0922\u093C\u0928\u0947\u2026"
lastmod: '2024-03-13T22:44:52.402550-06:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u092A\u0943\u0937\u094D\
  \u0920 \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0938\
  \u094D\u0925\u093E\u0928\u0940\u092F \u0938\u093F\u0938\u094D\u091F\u092E \u092A\
  \u0930 \u0938\u0939\u0947\u091C\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\u091F\
  \u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0935\u0947\u092C \u0938\
  \u094D\u0915\u094D\u0930\u0948\u092A\u093F\u0902\u0917, \u092F\u093E \u0911\u092B\
  \u0932\u093E\u0907\u0928 \u092A\u0922\u093C\u0928\u0947\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

वेब पेज डाउनलोड करने का मतलब है इंटरनेट से पृष्ठ की सामग्री को स्थानीय सिस्टम पर सहेजना। प्रोग्रामर्स इसे डेटा एनालिसिस, वेब स्क्रैपिंग, या ऑफलाइन पढ़ने के लिए करते हैं।

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
