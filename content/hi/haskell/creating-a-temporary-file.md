---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अस्थायी फ़ाइल एक फ़ाइल होती है जिसे किसी कार्य के लिए तात्कालिक उपयोग के लिए बनाया जाता है। यह तब आवश्यक होता है जब हमें डाटा को सुरक्षित रखना होता है या बाद में पुन: उपयोग करने के लिए बचाना होता है।

## कैसे करें:

Haskell में, `System.IO.Temp` मॉड्यूल का उपयोग करके अस्थायी फ़ाइलों का निर्माण किया जा सकता है।

```Haskell
import System.IO.Temp

main = withSystemTempFile "myTempFile.txt" $ \tempFilePath tempFileHandle -> do
    putStrLn $ "अस्थायी फ़ाइल " ++ tempFilePath ++ " बनाई गई है।"
```

उपरोक्त कोड एक अस्थायी फ़ाइल बनाता है और इसका पथ प्रिंट करता है।

## गहराई में:

Haskell में, `System.IO.Temp` मॉडयूल 2010 के आस-पास समावेशित हुआ। इसके विकल्प रूप में, आप 'System.Directory' का उपयोग कर सकते हैं, लेकिन यहां आपको फ़ाइल के निर्माण और मिटाने की जिम्मेदारी होती है। `withSystemTempFile` का उपयोग करना यह सुनिश्चित करता है कि अस्थायी फ़ाइल स्वचालित रूप से मिटाई जा सकती है।

## देखें भी:

1. [Haskell डॉक्स: System.IO.Temp](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
2. [Haskell पुस्तक: फ़ाइल प्रणाली](https://www.haskell.org/tutorial/io.html)