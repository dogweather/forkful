---
title:                "Haskell: नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप ने कभी सोचा है कि क्या हैं वे रेगुलर एक्सप्रेशन्स और आप इनका उपयोग क्यों करेंगे? इस ब्लॉग पोस्ट में, हम आपको हैस्कल में रेगुलर एक्सप्रेशन्स का उपयोग करने के कारण बताएंगे।

## कैसे

हैस्कल में रेगुलर एक्सप्रेशन्स का उपयोग बहुत ही आसान है। नीचे उदाहरण सहित हैस्कल कोड ब्लॉक में हमने बताया है कि आप कैसे एक सरल रेगुलर एक्सप्रेशन का उपयोग कर सकते हैं और उसके आउटपुट क्या होगा।

```Haskell
-- सरल रेगुलर एक्सप्रेशन उदाहरण
import Text.Regex.Posix

-- एक स्ट्रिंग से प्रथम अंक पाएं
findFirstNumber :: String -> Maybe String
findFirstNumber str = (=~"[0-9]+") str

main = do
  let str = "a1b2c3"
  print $ findFirstNumber str
```

आउटपुट:
```
Just "1"
```

## गहराई तक

हमारे पास रेगुलर एक्सप्रेशन्स काफी गहराई तक जानकारी है। आप हैस्कल में समान पैटर्न को पकड़ने के लिए विभिन्न फंक्शन जैसे (=~), (=~~), (=~~~) आदि का उपयोग कर सकते हैं। इनके अलावा, आप भी स्ट्रिंग पर अनेक पैटर्न को एक साथ चेक करने के लिए ग्रुपिंग और बैकरेफ्रंस का उपयोग कर सकते हैं। यह बेहद उपयोगी है जब आपको अनेक पैटर्न को पकड़ना हो और आप उन पैटर्न्स के क्रम को रखना चाहते हो।

## देखें भी

- [Haskell Wiki - Regular Expressions](https://wiki.haskell.org/Regular_expressions)
- [A Gentle Introduction to Regular Expressions in Haskell](https://haskell.fpcomplete.com/library/doc/parsing-and-regex)
- [Haskell Regular Expressions Cheat Sheet](https://devhints.io/haskell-regex)
- [Online Haskell Regular Expression Tester](https://regex-haskell.com/)