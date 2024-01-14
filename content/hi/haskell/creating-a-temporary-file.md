---
title:                "Haskell: अस्थायी फाइल बनाना"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप एक हैस्केल प्रोग्रामर हैं तो आपने temporary files के बारे में सुना होगा। ये एक common programming practice है जो हमें ध्यान रखना चाहिए। इस लेख में हम temporary files के बारे में गहराई से बात करेंगे।

## कैसे करें

यदि आप हास्केल में temporary files बनाना चाहते हैं तो आपको `System.IO.Temp` मॉड्यूल का उपयोग करना होगा। ये मॉड्यूल temporary फ़ाइल को create, read और delete करने के लिए फ़ंक्शन प्रदान करता है।

आइए हम एक example देखें जहाँ हम temporary file बनाकर उसमें कुछ लिखते हैं और फिर उसको पढ़ते हैं।

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents)

main = withSystemTempFile "example.txt" $ \path handle -> do
    hPutStrLn handle "नमस्ते दुनिया!"
    hGetContents handle >>= putStrLn
```

इस कोड का output नीचे दिया गया है:

```
नमस्ते दुनिया!
```

इस code में हमने `withSystemTempFile` फ़ंक्शन का उपयोग किया है जो दो arguments लेता है - एक temporary फ़ाइल का prefix और एक callback function। यह callback function temporary फ़ाइल का path और handle को लेता है जिसका हम उपयोग कर सकते हैं। हमने `hPutStrLn` function का उपयोग करके temporary फ़ाइल में लिखा है और फिर `hGetContents` फ़ंक्शन का उपयोग करके उसमें से text को पढ़ा है।

## गहराई में

Temporary files को create करने के कई तरीके हैं लेकिन वे सभी कुछ common features share करते हैं। जब temporary file create होता है तो वो system के default लोकेशन में जाता है। यदि आप इस default लोकेशन को override करना चाहते हैं तो आप `withSystemTempDirectory` फ़ंक्शन का उपयोग कर सकते हैं। भी temporary file को delete करने के लिए `removeFile` function का उपयोग कर सकते हैं।

एक ऐसा