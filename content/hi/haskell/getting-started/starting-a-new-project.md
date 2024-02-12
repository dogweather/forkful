---
title:                "नई परियोजना शुरू करना"
aliases:
- /hi/haskell/starting-a-new-project/
date:                  2024-01-20T18:04:49.032049-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना मतलब है खाली कैनवास पर अपनी क्रिएटिविटी उतारना। प्रोग्रामर इसलिए नए प्रोजेक्ट्स शुरू करते हैं क्योंकि नई समस्याओं का हल निकालना, सीखना, और नए टूल्स बनाना उनके लिए चुनौतीपूर्ण और रोमांचक होता है।

## How to: (कैसे करें:)
Haskell में नया प्रोजेक्ट शुरू करने के लिए, हम Cabal और Stack, दोनों टूल्स का इस्तेमाल कर सकते हैं। पहले आपको Haskell का सेटअप करना होगा। यहां पर सिम्पल कोड है:

```Haskell
-- हेलो वर्ल्ड हास्केल प्रोग्राम

main :: IO ()
main = putStrLn "नमस्ते दुनिया!"
```

सैंपल आउटपुट:

```
नमस्ते दुनिया!
```

## Deep Dive (गहराई में जानकारी):
Haskell एक फंक्शनल प्रोग्रामिंग भाषा है जिस का विकास 1990 में हुआ था। इसका इस्तेमाल सॉफ्टवेयर पैकेज मेनेजमेंट के लिए कैबल (Cabal) और स्टैक (Stack) जैसे टूल्स के साथ होता है, जो प्रोजेक्ट की निर्भरताओं और संस्करण प्रबंधन को आसान बनाते हैं। इन टूल्स के बिना, Haskell प्रोजेक्ट्स को मैनेज करना काफी मुश्किल हो सकता है। 

Cabal और Stack विभिन्न तरह के हैं, लेकिन दोनों का मकसद प्रोजेक्ट्स को आसानी से बनाना और मैनेज करना है। Cabal जहां सॉफ्टवेयर लायब्रेरीज के लिए पैकेज मेनेजमेंट प्रदान करता है, वहीं Stack दिए गए पैकेज वर्कफ्लो और संस्करण को स्थिर और संगत बनाता है।

## See Also (देखें यह भी):
- Haskell ऑफिशल साइट: [https://www.haskell.org/](https://www.haskell.org/)
- Learn You a Haskell (एक मुफ्त गाइड): [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- Cabal यूजर गाइड: [https://cabal.readthedocs.io/en/latest/](https://cabal.readthedocs.io/en/latest/)
- प्रोजेक्ट्स के लिए स्टैक का इस्तेमाल: [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
