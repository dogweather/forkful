---
date: 2024-01-26 04:33:08.994252-07:00
description: "\u0915\u0948\u0938\u0947: Haskell `xml-conduit` \u091C\u0948\u0938\u0940\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948 \u091C\u094B XML \u0938\u0947\
  \ \u0928\u093F\u092A\u091F\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\
  \u0964 \u0928\u093F\u092E\u094D\u0928 \u0909\u0926\u093E\u0939\u0930\u0923 \u090F\
  \u0915 XML \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0914\u0930 \u0924\u0924\u094D\u0935\
  \u094B\u0902 \u0915\u094B \u092A\u0942\u091B\u0924\u093E\u091B \u0915\u0930\u0928\
  \u0947\u2026"
lastmod: '2024-03-13T22:44:52.446561-06:00'
model: gpt-4-0125-preview
summary: "Haskell `xml-conduit` \u091C\u0948\u0938\u0940 \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\
  \u093E \u0939\u0948 \u091C\u094B XML \u0938\u0947 \u0928\u093F\u092A\u091F\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\u0964 \u0928\u093F\u092E\u094D\
  \u0928 \u0909\u0926\u093E\u0939\u0930\u0923 \u090F\u0915 XML \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u0947 \u0914\u0930 \u0924\u0924\u094D\u0935\u094B\u0902 \u0915\u094B \u092A\
  \u0942\u091B\u0924\u093E\u091B \u0915\u0930\u0928\u0947 \u0915\u093E \u092A\u094D\
  \u0930\u0926\u0930\u094D\u0936\u0928 \u0915\u0930\u0924\u093E \u0939\u0948."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

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
