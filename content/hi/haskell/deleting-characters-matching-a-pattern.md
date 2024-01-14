---
title:                "Haskell: पैटर्न के समान अक्षरों को हटाना"
simple_title:         "पैटर्न के समान अक्षरों को हटाना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Haskell में कैसे समान पैटर्न को मैचिंग करने वाले अक्षरों को हटाना

## क्यों

कभी-कभी हमें अपने डेटा से प्रतीक को हटाने की जरूरत होती है, जैसे कि अपने एक टेक्स्ट स्ट्रिंग से सभी नंबर्स हटाने की जरूरत हो सकती है। हैस्केल में, हम विशेष पैटर्न मैचिंग या रिग्यूलर एक्सप्रेशन का उपयोग करके इसे आसानी से कर सकते हैं।

## कैसे

हम अपनी स्ट्रिंग को बहुत सारे तरीकों से मैच कर सकते हैं। सबसे आसान तरीका है `filter` फ़ंक्शन का उपयोग करना। नीचे दिए गए कोड ब्लॉक में, हम `filter` का उपयोग करके स्ट्रिंग से सभी अक्षर हटा रहे हैं जो एक से निकले हुए हैं।

```Haskell
removeDuplicates :: String -> String
removeDuplicates str = filter (\x -> x /= head str) str
```

इसका आउटपुट निम्न रहेगा:

```Haskell
removeDuplicates "abbcdd"
"ac"
```

## गहराई में जाएं

हैस्केल में, हम `filter` के अलावा भी अन्य तरीकों से स्ट्रिंग मैच कर सकते हैं। एक अन्य तरीका है `map` फ़ंक्शन का उपयोग करना जिससे कि हम स्ट्रिंग को बदल सकते हैं।

```Haskell
removeLowerCase :: String -> String
removeLowerCase str = map (\x -> if isLower x then ' ' else x) str
```

इसका आउटपुट निम्न रहेगा:

```Haskell
removeLowerCase "Haskell"
"H "
```

## देखें भी

- [Haskell विकिपीडिया पृष्ठ](https://hi.wikipedia.org/wiki/%E0%A4%B9%E0%A5%87%E0%A4%B8%E0%A5%8D%E0%A4%95%E0%A5%87%E0%A4%B2)
- [Haskell फंक्शनल प्रोग्रामिंग कोडिंग विकीबुक](https://hi.wikibooks.org/wiki/Haskell_%E0%A4%AB%E0%A5%82%E0%A4%82%E0%A4%95%E0%A5%8D%E0%A4%B8%E0%A4%A8%E0%A4%B