---
title:                "Haskell: प्रिंटिंग डीबग आउटपुट"
simple_title:         "प्रिंटिंग डीबग आउटपुट"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

डिबग आउटपुट प्रिंट करना हमारे ऐप्लिकेशन को डिबग करने का एक आसान तरीका है। यह हमेशा मददगार होता है जब हमें कोड में गड़बड़ी का पता लगाना होता है। डिबग आउटपुट हमें बेहतर समझने और ठीक करने में मदद करता है। 

## कैसे करें

```Haskell
-- यह उदाहरण आपको संख्याओं को जोड़ने का कोड दिखाता है
main :: IO ()
main = do
  let x = 5
  let y = 10
  putStrLn("x का मान:" ++ show x)
  putStrLn("y का मान:" ++ show y)
  putStrLn("जोड़:" ++ show (x + y))
```

```
x का मान: 5
y का मान: 10
जोड़: 15
```

उपरोक्त उदाहरण में, हमने आउटपुट में विभिन्न संख्याओं का मान प्रिंट किया है जो हमने अपने प्रोग्राम में डिबग करने के लिए उपयोग किया है। हमने `show` फंक्शन का उपयोग करके हमारे मानों को स्ट्रिंग में रूपांतरित किया है जिससे हम उसे `putStrLn` फंक्शन के साथ उपयोग कर सकें। 

## गहराई से जाएं

डिबग आउटपुट का उपयोग करना कोड में समस्याओं का पता लगाने के लिए बहुत ही उपयोगी होता है। यह हमें अपने कोड को समझने के लिए मदद करता है और सही समाधान प्राप्त करने में मदद करता है। डिबग आउटपुट का उपयोग करना आसान है और हमेशा एक बेहतर सुराग देता है। 

## देखें भी

- [Haskell के साथ मज़ेदार कोडिंग के टिप्स](https://www.geeksforgeeks.org/tips-for-fast-coding-with-haskell/)
- [Haskell डिबगिंग की पूरी गाइड](https://dev.to/hansus/haskell-debugging-the-complete-guide-53ef)