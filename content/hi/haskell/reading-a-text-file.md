---
title:    "Haskell: टेक्स्ट फ़ाइल पढ़ना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फाइल पढ़ने में रुचि लेने का कारण है कि यह भाषा ढोंगी नहीं है और यह भाषा संरचित और पढ़ने में सरल है।

## कैसे करें

कैसे टेक्स्ट फाइल को पढ़ने और कोडिंग करने के लिए हस्केल का उपयोग करें, देखें:

```Haskell
-- फाइल से डेटा पढ़ें
main = do
  myFile <- readFile "example.txt"
  putStrLn myFile

-- "example.txt" का उपयोग करने के लिए अपने फ़ाइल के नाम का उपयोग करें
```

आउटपुट:

```Haskell
This is an example text file.
It is used to demonstrate how to read a text file using Haskell.
```

कोड के संग्रहीत डेटा के साथ, अपनी आवश्यकताओं और प्रोग्राम के अनुसार, आप अन्य विकल्प भी उपयोग कर सकते हैं, जैसे कि `hGetContents` का उपयोग करके स्ट्रिंग वस्तु को बदलने के लिए।

## गहराई में गोता-जोड़

अब जानते हैं कि कैसे बस एक कदम में टेक्स्ट फाइल पढ़ी जा सकती है, आप हस्केल और स्ट्रिंग के साथ टेक्स्ट फाइलों के संबंधों को और गहराई से जान सकते हैं। आप डेटा को अपने अनुभव के अनुसार उपयोग कर सकते हैं और उससे संबंधित काम कर सकते हैं। अगर आप और गहराई की खोज करने की इच्छा रखते हैं, तो आप सामान्य फाइल पढ़ने फंक्शन के साथ और अधिक विकल्पों का भी उपयोग कर सकते हैं।

## और देखें

- [एक हस्केल विधि से टेक्स्ट फाइल पढ़ना](https://wiki.haskell.org/Handling_files)
- [हस्केल फाइल संबंधित सुझाव](https://www.codementor.io/@flavianunes/haskell-io-problems-tackled-with-io-monads-part-1-y9