---
title:    "Haskell: स्ट्रिंग को कैपिटलाइज करना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपने कोड में स्ट्रिंग को बड़े अक्षर में लिखने की जरूरत होती है। हास्केल में इसके लिए आसान तरीके है जो कि हम इस लेख में समझेंगे।

## कैसे करें

हास्केल में स्ट्रिंग को बड़े अक्षर में लिखने के लिए `toUpper` फ़ंक्शन का इस्तेमाल कर सकते हैं। इसका उपयोग करने का सिंटैक्स निम्न है:

```Haskell
toUpper "haskell"
```

और यह आउटपुट होगा:

```Haskell
"HASKELL"
```

आप `toUpper` फ़ंक्शन को अपने कोड में इस तरह से भी इस्तेमाल कर सकते हैं:

```Haskell
capitalize :: String -> String
capitalize str = toUpper str
```

इस उदाहरण में, हमने एक `capitalize` नाम का फ़ंक्शन बनाया है जो कि स्ट्रिंग को बड़े अक्षर में लिखने का काम करता है।

## गहराई में खोज

हास्केल में स्ट्रिंग को बड़े अक्षर में लिखने के पीछे की तकनीक अंततः यह है कि `toUpper` 2 आर्गूमेंट पर स्वीकार करता है - `Locale` और `TextEncoding`। जब हम इस फ़ंक्शन को उपयोग में लाते हैं, तो हम एक 'default locale' दे सकते हैं और सिस्टम की डिफ़ॉल्ट टेक्स्ट एन्कोडिंग का उपयोग कर सकते हैं।

अतिरिक्त गहरी जानकारी के लिए, आप [Haskell String Module documentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#g:8) देख सकते हैं।

## इसके अलावा

- [Haskell Tutorials in Hindi](https://www.haskelltutorialsinhindi.com/)
- [Haskell भाषाओं में हिन्दी भाषा का स्वागत](http://harry.me/blog/8)
- [Haskell Hindi Groups on Github](https://github.com/HaskellMumbai/haskell-prez/wiki/Hindi-groups)