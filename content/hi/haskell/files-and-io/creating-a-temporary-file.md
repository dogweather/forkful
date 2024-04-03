---
date: 2024-01-20 17:40:52.725939-07:00
description: "\u0915\u0948\u0938\u0947: ."
lastmod: '2024-03-13T22:44:52.438282-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## कैसे:
```haskell
import System.IO

-- टेम्परेरी फाइल बनाएं और उसमें लिखें
main :: IO ()
main = do
  (tempFilePath, tempHandle) <- openTempFile "." "temp"
  hPutStrLn tempHandle "यह कुछ अस्थायी डाटा है"
  hClose tempHandle
  putStrLn $ "टेम्परेरी फाइल बनाई गयी: " ++ tempFilePath
```

सैंपल आउटपुट:
```
टेम्परेरी फाइल बनाई गयी: ./temp1234.txt
```

## गहराई से जानकारी
अस्थायी फाइलें उनकी सामयिकता के लिए प्रसिद्ध हैं। यह अवधारणा Unix-like सिस्टम्स से आई है, जहां `/tmp` डायरेक्टरी अस्थायी फाइलों के लिए होती है। हास्केल में, `System.IO` मॉड्यूल का उपयोग करके आसानी से टेम्परेरी फाइलें बनाई जा सकती हैं। इसके विकल्प के तौर पर, `temporary` पैकेज भी है जो अधिक सुविधाएँ और अधिक नियंत्रण प्रदान करता है। टेम्परेरी फाइल्स सुरक्षा के दृष्टिकोण से भी महत्वपूर्ण हो सकती हैं, क्योंकि वे केवल उस समय के लिए मौजूद रहती हैं जब तक उनकी जरूरत होती है और फिर स्वचालित रूप से साफ हो जाती हैं।

## और भी जानें
- Haskell `System.IO` Module: [System.IO Documentation](https://hackage.haskell.org/package/base/docs/System-IO.html)
- टेम्परेरी फाइल्स के लिए `temporary` पैकेज: [temporary Package](https://hackage.haskell.org/package/temporary)
- सुरक्षित टेम्परेरी फाइल्स पर लेख: [Secure Temp File Creation](https://www.owasp.org/index.php/Insecure_Temporary_File)
