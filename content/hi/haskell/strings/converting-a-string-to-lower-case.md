---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases: - /hi/haskell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:45.546700-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? | क्या और क्यों?
एक स्ट्रिंग को लोअर केस में बदलने का मतलब है उसके सभी अक्षरों को छोटे अक्षर में बदलना। प्रोग्रामर्स यह तब करते हैं जब वे टेक्स्ट डेटा की तुलना करते समय मामले (case) की अनदेखी करना चाहते हैं।

## How to: | कैसे:
```Haskell
import Data.Char (toLower)

-- स्ट्रिंग को लोअर केस में बदलने का फंक्शन
lowerCaseStr :: String -> String
lowerCaseStr = map toLower

-- उपयोग का उदाहरण
main :: IO ()
main = putStrLn (lowerCaseStr "Hello नमस्ते!")

-- सैंपल आउटपुट: "hello नमस्ते!"
```

उपरोक्त कोड `Data.Char` मॉड्यूल के `toLower` फंक्शन का उपयोग करके स्ट्रिंग को लोअर केस में बदलता है।

## Deep Dive | गहराई में जानकारी
स्ट्रिंग को लोअर केस में बदलना पुराने समय से प्रोग्रामिंग में एक सामान्य क्रिया है, जो टेक्स्ट प्रोसेसिंग और नॉर्मलाइजेशन के लिए उपयोग होती है। Haskell में `Data.Char` मॉड्यूल यह कार्य सरलता से करता है, पर इसमें अन्य मॉड्यूल्स जैसे `Data.Text` भी लोअरकेसिंग के लिए उपलब्ध हैं, खासकर बड़े टेक्स्ट ब्लॉक्स के लिए।

`toLower` फंक्शन Unicode स्टैंडर्ड का पालन करता है, जो अधिकतर भाषाओं में छोटे और बड़े अक्षरों के बीच मैपिंग करता है। कुछ भाषाओं में अक्षर केस कन्वर्जन की जटिलताएँ हो सकती हैं, जिसके लिए विशेष परिस्थितियों में विशेष लाइब्रेरीज़ या अलग लॉजिक की आवश्यकता हो सकती है।

## See Also | देखें भी
- Haskell `Data.Char` Module: [Hackage Documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html)
- Unicode Case Mapping: [Unicode Standard](https://www.unicode.org/reports/tr21/tr21-5.html)

इन लिंक्स पर जाकर, आप Haskell में टेक्स्ट प्रोसेसिंग और `toLower` फंक्शन के बारे में और जान सकते हैं।
