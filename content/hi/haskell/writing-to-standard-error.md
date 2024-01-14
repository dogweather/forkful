---
title:                "Haskell: स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों 

वैसे तो हैस्केल में स्टैंडर्ड इरर पर लिखने का उपयोग दोस्ताना नहीं है, लेकिन कभी-कभी हमारे कोड में गलतियां आ सकती हैं जो कि हमारे एप्लिकेशन को डूबाने की बजाय हमेशा स्टैंडर्ड एरर पर लिखने के माध्यम से धीमा कर सकती हैं।

## कैसे करें

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "यह स्टैंडर्ड इरर पर लिखने का उदाहरण है"
    hPutStrLn stderr "और यह कुछ गलत संदेश हैं |"
    let a = 5 :: Int
        b = 0 :: Int
    if b /= 0
        then do
            let result = a `div` b
            hPutStrLn stderr $ "विभाजन: " ++ show result
        else hPutStrLn stderr "गलत: विभाजन 0 से हो रहा है |"
```

## गहराई से जानें

स्टैंडर्ड इरर दर्शाने वाले यूजर को गलतियों के बारे में जानकारी देने के लिए बहुत ही उपयोगी है। साथ ही, यह कोड को डीबग करने में भी महत्वपूर्ण भूमिका निभाता है। हास्कल में हम `hPutStrLn` फंक्शन का उपयोग करते हैं जो एक स्ट्रिंग को स्टैंडर्ड इरर पर लिखता है। हम भी डेटा टाइप ट्रैस की सहायता से अपने संदेश को स्लॉट करते हैं ताकि उसे लगभग किसी भी डेटा के साथ मिला सकें।

## देखें भी

- [Haskell में एरर्स पर काम करने का सबसे सही तरीका](https://www.learnyouahaskell.com/for-a-few-monads-more#error)
- [Haskell स्टैंडर्ड लाइब्रेरी: `System.IO`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)