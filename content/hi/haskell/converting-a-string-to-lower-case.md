---
title:                "स्ट्रिंग को लोअर केस में रूपांतरित करना"
html_title:           "Haskell: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोन्टेक्स्ट से बाहर आते हुए हम कस्टम अलग्योरिदम को इस्तेमाल करके स्ट्रिंग को लोअर केस में बदलना चाहते हैं। यह काम उस स्थिति में किया जाता है जब हमें किसी अन्य नामों की बजाय स्ट्रिंग को स्थानांतरित करना हो।

## कैसे करें?
```Haskell
toLower :: Char -> Char -- Char को Used करके एक Char टाइप को लोअर केस में बदलता है 
map toLower "HELLO Haskell!" -- "hello haskell!"
```

## गहराई में
किसी भी भाषा में स्ट्रिंग को लोअर केस में बदलना बहुत आसान है। आपको कनवर्टिंग के लिए प्रोग्रामिंग भाषा की कोई क्षमता की जरूरत नहीं है। हैस्केल में, हम इस कार्य को आसानी से `map` फंक्शन के माध्यम से कर सकते हैं।

## और भी देखें
- [Haskell Wiki - String Manipulation](https://wiki.haskell.org/String_manipulation)
- [GeeksforGeeks - Convert String to Lowercase in Haskell](https://www.geeksforgeeks.org/convert-a-string-to-lower-case-in-haskell/)