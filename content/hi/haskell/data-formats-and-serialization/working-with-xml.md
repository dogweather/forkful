---
title:                "XML के साथ काम करना"
aliases:
- /hi/haskell/working-with-xml.md
date:                  2024-01-26T04:33:08.994252-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Haskell में XML के साथ काम करना XML संरचनाओं को पार्स करना, हेरफेर करना, और उत्पन्न करना शामिल है। प्रोग्रामर XML को कई अनुप्रयोगों और प्रोटोकॉलों के साथ बातचीत करने के लिए संभालते हैं जो XML को अपने डेटा प्रारूप के रूप में उपयोग करते हैं, जैसे कि वेब सेवाएँ और कॉन्फ़िगरेशन फ़ाइलें।

## कैसे:

Haskell `xml-conduit` जैसी लाइब्रेरी प्रदान करता है जो XML से निपटने के लिए है। निम्न उदाहरण एक XML स्ट्रिंग को पार्स करने और तत्वों को पूछताछ करने का प्रदर्शन करता है:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

नमूना आउटपुट:

```
["World!"]
```

## गहराई से अध्ययन

XML, जिसका पूरा नाम eXtensible Markup Language है, JSON के उदय से बहुत पहले से ही डेटा सीरियलाइजेशन में एक स्थायी स्थिति रखता है। यह वर्बोस है, लेकिन कठोर और मानकीकृत है, जिससे यह सख्त उद्यम वातावरणों, पुरानी प्रणालियों और वित्त और स्वास्थ्य सेवा जैसे उद्यो�

Haskell में XML के लिए कई लाइब्रेरियां हैं; हालाँकि, `xml-conduit` इसकी कुशल स्ट्रीमिंग और पार्सिंग क्षमताओं के कारण सबसे शक्तिशाली और व्यापक रूप से इस्तेमाल की जाने वाली लाइब्रेरी में से एक है, जो डेटा स्ट्रीम्स को संभालने के लिए `conduit` परिवार का हिस्सा है।

विकल्पों में `HXT` (Haskell XML Toolbox) शामिल है जो पार्सिंग और परिवर्तन के लिए तीरों का उपयोग करता है, XML हेरफेरों के लिए एक अलग पैराडाइम प्रदान करता है। हालाँकि `HXT` अब इसकी सीखने की अधिक ढलान के कारण कम लोकप्रिय है, यह कुछ उपयोग के मामलों के लिए अभी भी एक मजबूत विकल्प रहता है।

Haskell में XML प्रोसेसिंग को लागू करते समय, आपको एन्कोडिंग के बारे में चिंता करनी होगी, क्योंकि Haskell तार यूनिकोड होते हैं और XML डेटा नहीं हो सकता है। अतिरिक्त रूप से, XML नामस्थान पार्सिंग में अतिरिक्त जटिलता जोड़ सकते हैं।

## देखें भी:

- `xml-conduit` पैकेज डॉक्यूमेंटेशन: https://hackage.haskell.org/package/xml-conduit
- Haskell XML टूलबॉक्स (HXT): http://hackage.haskell.org/package/hxt
- "Real World Haskell" पुस्तक, अध्याय 16, XML हैंडलिंग के लिए: http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki पर XML: https://wiki.haskell.org/XML
