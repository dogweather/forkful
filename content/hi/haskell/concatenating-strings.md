---
title:    "Haskell: स्ट्रिंग्स को जोड़ना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों
Haskell में दो या अधिक स्ट्रिंग्स का जोड़ना सामान्य रूप से एक आवश्यकता होती है। इससे हमें कई भिन्न प्रकार के डेटा को सम्मिलित करने और उन्हें एक बिंदु पर प्रकाशित करने का मौका मिलता है।

## कैसे करें
```Haskell
-- स्ट्रिंग्स को जोड़ने का सरल तरीका
"Hello" ++ "World"
-- आउटपुट: "HelloWorld"

-- यदि हम कुछ अतिरिक्त क्रम में स्ट्रिंग्स को जोड़ना चाहते हैं
"Hello" ++ " " ++ "Haskell" ++ " " ++ "World"
-- आउटपुट: "Hello Haskell World"
```

## गहराई में जाएं
स्ट्रिंग्स को `++` ऑपरेटर का उपयोग करके जोड़े जाने के अलावा, हम  `concat` फ़ंक्शन का भी उपयोग कर सकते हैं जो दो स्ट्रिंग्स को जोड़कर एक नया स्ट्रिंग बनाता है। इसके अलावा, हम अलग-अलग स्ट्रिंग अर्ग्यूमेंट्स को `++` ऑपरेटर के माध्यम से अनुक्रम में जोड़ सकते हैं।

## देखें भी
- [Haskell स्ट्रिंग्स](https://www.geeksforgeeks.org/haskell-strings/)
- [Haskell स्ट्रिंग ऑपरेटर्स](https://www.haskell.org/tutorial/strings.html)
- [Haskell कीवर्ड और सिंटैक्स](https://www.haskell.org/syntax/)