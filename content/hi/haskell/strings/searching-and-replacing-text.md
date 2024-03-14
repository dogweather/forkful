---
date: 2024-01-20 17:58:13.456278-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\u093F\
  \u0902\u0917 \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938\u093F\u0902\
  \u0917 \u092F\u093E\u0928\u0940 \u0915\u093F \u0915\u093F\u0938\u0940 \u0926\u0938\
  \u094D\u0924\u093E\u0935\u0947\u091C\u093C \u092E\u0947\u0902 \u0936\u092C\u094D\
  \u0926\u094B\u0902 \u0915\u094B \u0922\u0942\u0901\u0922\u0928\u093E \u0914\u0930\
  \ \u091C\u0930\u0941\u0930\u0924 \u092A\u0922\u093C\u0928\u0947 \u092A\u0930 \u0909\
  \u0928\u094D\u0939\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 \u0921\u093E\u091F\u093E \u0938\u0902\
  \u0936\u094B\u0927\u0928, \u0924\u094D\u0930\u0941\u091F\u093F\u2026"
lastmod: '2024-03-13T22:44:52.378955-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\u093F\
  \u0902\u0917 \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938\u093F\u0902\
  \u0917 \u092F\u093E\u0928\u0940 \u0915\u093F \u0915\u093F\u0938\u0940 \u0926\u0938\
  \u094D\u0924\u093E\u0935\u0947\u091C\u093C \u092E\u0947\u0902 \u0936\u092C\u094D\
  \u0926\u094B\u0902 \u0915\u094B \u0922\u0942\u0901\u0922\u0928\u093E \u0914\u0930\
  \ \u091C\u0930\u0941\u0930\u0924 \u092A\u0922\u093C\u0928\u0947 \u092A\u0930 \u0909\
  \u0928\u094D\u0939\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 \u0921\u093E\u091F\u093E \u0938\u0902\
  \u0936\u094B\u0927\u0928, \u0924\u094D\u0930\u0941\u091F\u093F\u2026"
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
टेक्स्ट सर्चिंग और रिप्लेसिंग यानी कि किसी दस्तावेज़ में शब्दों को ढूँढना और जरुरत पढ़ने पर उन्हें बदलना। प्रोग्रामर्स इसे आमतौर पर डाटा संशोधन, त्रुटि सुधार या पुनः प्रयोग के लिए करते हैं।

## कैसे करें? (How to:)
```Haskell
import Data.List (isInfixOf)

-- टेक्स्ट सर्च फंक्शन
searchText :: String -> String -> Bool
searchText = isInfixOf

-- टेक्स्ट रिप्लेस फंक्शन
replaceText :: Eq a => [a] -> [a] -> [a] -> [a]
replaceText [] _ _ = []
replaceText s find repl = if take (length find) s == find
                          then repl ++ replaceText (drop (length find) s) find repl
                          else head s : replaceText (tail s) find repl

-- उदाहरण का उपयोग
main :: IO ()
main = do
    let text = "नमस्ते हास्केल दुनिया"
    let searchTextResult = searchText "हास्केल" text
    print searchTextResult -- आउटपुट: True

    let replaceTextResult = replaceText text "हास्केल" "वर्ल्ड"
    putStrLn replaceTextResult -- आउटपुट: नमस्ते वर्ल्ड दुनिया
```

## गहराई से विचार (Deep Dive)
टेक्स्ट सर्चिंग और रिप्लेसिंग हास्केल जैसी फंक्शनल लैंग्वेज़ में ज्यादा सुव्यवस्थित और सुरक्षित है बजाए दूसरी भाषाओं की। ऐतिहासिक रूप से, यह कार्य बैच प्रोसेसिंग और एडिटर स्क्रिप्टिंग में उपयोगी रहा है। रिप्लेसमेंट फंक्शन के लिए हास्केल डेटा टाइप की सख्ती और पैटर्न मैचिंग का लाभ लेता है, जो कि एरर को कम करता है। विकल्प में, रेगुलर एक्सप्रेशंस का उपयोग भी हास्केल में `Text.Regex` पैकेज के साथ किया जा सकता है।

## देखें भी (See Also)
- हास्केल फंक्शनल प्रोग्रामिंग की अवधारणा: [Haskell.org](https://www.haskell.org/)
- `Data.List` मॉड्यूल के दस्तावेज़: [Hackage Data.List](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html)
- `Text.Regex` पैकेज का उपयोग: [Hackage Text.Regex](https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html)
