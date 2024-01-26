---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:32.095286-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
पैटर्न से मेल खाने वाले वर्णों को हटाना एक प्रोग्रामिंग तकनीक है जिसमें आप एक निश्चित पैटर्न द्वारा पहचाने गए अक्षरों को एक टेक्स्ट स्ट्रिंग से हटा देते हैं। डाटा सफाई, टेक्स्ट प्रोसेसिंग या इनपुट वैलिडेशन जैसे काम में इसकी ज़रूरत पड़ती है।

## कैसे करें:
Haskell में पैटर्न से मेल खाने वाले वर्णों को हटाने के लिए एक सीधा उदाहरण `Data.List` मॉड्यूल का `filter` फंक्शन है:

```haskell
import Data.List (isInfixOf, filter)

-- 'filter' का उपयोग करके पैटर्न हटाना
removePattern :: String -> String -> String
removePattern pattern = filter (not . (`isInfixOf` pattern) . (:[]))

main :: IO ()
main = do
  let text = "hello world"
  let pattern = "lo"
  putStrLn $ removePattern pattern text -- "he wor"
```

यहां, `removePattern` फंक्शन पैटर्न न मिलने वाले वर्णों को छोड़ कर देता है, ताकि आपकी स्ट्रिंग से वो भाग हट जाए जो मौकेपर नहीं चाहिए।

## गहराई से जानकारी:
हास्केल प्रोग्रामर्स डाटा को संसाधित करते समय अक्सर पैटर्न मैचिंग का उपयोग करते हैं। 1990 के दशक की शुरुआत में हास्केल की रचना के बाद से, फंक्शनल प्रोग्रामिंग पैराडाइम में इस तरह की तकनीकों के लिए लगातार सुधार होते रहे हैं। `filter` के विकल्प के रूप में, हम `Data.Text` मॉड्यूल में अधिक कुशल फंक्शंस जैसे कि `replace` या `strip` का भी उपयोग कर सकते हैं। सीधे इम्प्लीमेंटेशन के बजाय, हास्केल लाइब्रेरीज कोड के पुनः उपयोग और बेहतर अभ्यास को बढ़ावा देती हैं। 

## और देखें:
यदि आप हास्केल में पैटर्न मैचिंग और टेक्स्ट प्रोसेसिंग के बारे में और जानना चाहते हैं, तो ये स्रोत उपयोगी हो सकते हैं:

- Real World Haskell की वेबसाइट: http://book.realworldhaskell.org/
- "Learn You a Haskell for Great Good!" वेबसाइट: http://learnyouahaskell.com/chapters
- Haskell.org पर उनका पैटर्न मैचिंग गाइड: https://www.haskell.org/tutorial/patterns.html
