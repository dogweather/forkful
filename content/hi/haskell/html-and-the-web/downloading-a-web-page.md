---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/haskell/downloading-a-web-page/
date:                  2024-01-20T17:44:14.436731-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/downloading-a-web-page.md"
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
