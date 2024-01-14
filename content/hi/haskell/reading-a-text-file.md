---
title:                "Haskell: टेक्स्ट फाइल को पढ़ना"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## इसका कारण

टेक्स्ट फाइल को पढ़ने का कारण आपको अपने हैस्केल कोड को सुधारने और अधिक सुस्त और सुव्यवस्थित बनाने के लिए हो सकता है।

## कैसे करें

टेक्स्ट फाइल को पढ़ने के लिए, आपको पहले से ही हैस्केल का `System.IO` मॉड्यूल इंपोर्ट करना होगा। यहां आपको कुछ उदाहरण दिए गए हैं:

```Haskell
-- फाइल को खोलें
file <- openFile "example.txt" ReadMode
-- लाइन पढ़ें
line <- hGetLine file
-- लाइन प्रिंट करें
putStrLn line
-- फाइल बंद करें
hClose file
```

सामान्यतः, हम `openFile` फंक्शन का उपयोग करते हैं ताकि हम फाइल को खोल और उसे पढ़ सकें। फाइल बंद करना बहुत महत्वपूर्ण है क्योंकि यह हमारे कंप्यूटर के लिए गंभीर स्थिति हो सकता है।

## गहराई में खोज

गहराई में एक टेक्स्ट फाइल को पढ़ना बहुत ही आवश्यक है जब आप अपने कोड से डेटा को प्रोसेस करने के लिए उसे लोड करना चाहते हैं। इसके अलावा, आप अपनी फाइल को मैनपुलेट करने के लिए भी इसका उपयोग कर सकते हैं।

## और देखो

- [हैस्केल में सिस्टम आईओ](https://wiki.haskell.org/System_IO)
- [Haskell Cookbook: फाइल्स को पढ़ना और लिखना](https://haskellcookbook.com/chapters/files.html)