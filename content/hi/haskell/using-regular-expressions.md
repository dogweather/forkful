---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रेगुलर एक्सप्रेशन्स या रेजेक्स पैटर्न मैचिंग का एक तरीका है जिससे टेक्स्ट को ढूँढा और मैनिपुलेट किया जा सकता है। प्रोग्रामर्स इसे कोड को सिंपलीफाई करने और तेज़ी से डेटा प्रोसेस करने के लिए इस्तेमाल करते हैं।

## कैसे करें:
Haskell में रेगुलर एक्सप्रेशन्स का इस्तेमाल करने के लिए `regex` पॅकेज होता है। इसे इंस्टॉल करने के बाद, आप पैटर्न मैचिंग कर सकतें हैं:

```Haskell
import Text.Regex.TDFA ((=~))

-- पैटर्न मैचिंग उदाहरण
main :: IO ()
main = do
  let text = "यहाँ कुछ टेक्स्ट है"
  let pattern = "कुछ"
  print $ text =~ pattern :: Bool
```
आउटपुट:
```
True
```

## गहराई से डाइव:
रेगुलर एक्सप्रेशन्स का इतिहास 1950 के दशक में शुरू हुआ था। अलग-अलग प्रोग्रामिंग भाषाओं और टूल्स में रेजेक्स का अपना-अपना वर्शन होता है। Haskell में, `regex` पॅकेज द्वारा उपलब्ध किे गए फ़ंक्शन्स परफॉर्मेंट होते हैं और पॉवरफुल पैटर्न मैचिंग प्रदान करते हैं। वैकल्पिक तौर पर, `string` और `text` पॅकेजेस में सिंपल सर्च ऑपरेशंस के फ़ंक्शन्स भी होते हैं जो बेसिक टेक्स्ट मैनिपुलेशन के लिए काफी होते हैं।

## देखें भी:
- Real World Haskell किताब: http://book.realworldhaskell.org/read/regular-expressions.html
- Learn You a Haskell for Great Good (रेजेक्स सेक्शन): http://learnyouahaskell.com/input-and-output#files-and-streams
- Hackage `regex-tdfa` पॅकेज: https://hackage.haskell.org/package/regex-tdfa
