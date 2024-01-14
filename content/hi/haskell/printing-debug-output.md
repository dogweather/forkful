---
title:                "Haskell: डिबग आउटपुट प्रिंट करना"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

कोडिंग में डीबग आउटपुट को प्रिंट करने का काम काफी महत्वपूर्ण है। यह आपको अपने कोड को समझने और समस्याओं को सुलझाने में मदद कर सकता है। 

## कैसे करें

हैस्केल में डीबग आउटपुट प्रिंट करने के लिए, आप निम्न उदाहरण द्वारा इसको कैसे करें जान सकते हैं:

```Haskell
main = do
  putStrLn "Hello, world!"
  putStrLn "Debug output: 123"
```

इसका आउटपुट निम्न प्रकार होगा:
```
Hello, world!
Debug output: 123
```

## गहराई में जाएं

डीबग आउटपुट प्रिंट करने के लिए आप सीधे `putStrLn` का उपयोग कर सकते हैं या फिर स्ट्रिंग संयोजन के माध्यम से इसे भी कर सकते हैं। आप संदर्भ के रूप में अपने स्किलेल और डीबग आउटपुट के स्तर के अनुसार उपयोग कर सकते हैं। 

## देखें भी

- [Haskell में फंक्शन्स को परीक्षण कैसे करें](https://www.haskelltutorials.in/haskell-tutorial/debugging-functions-testing-in-haskell)
- [Haskell में गार्ड और गार्ड्स कैसे काम करते हैं](https://www.haskelltutorials.in/haskell-tutorial/guards-and-guards-in-haskell)
- [Haskell के ऑनलाइन कोड कंपाइलर का उपयोग कैसे करें](https://www.haskelltutorials.in/haskell-tutorial/using-online-code-compiler-in-haskell)
- [Haskell के बेसिकस क्या होते हैं](https://www.haskelltutorials.in/haskell-tutorial/what-are-basics-in-haskell)