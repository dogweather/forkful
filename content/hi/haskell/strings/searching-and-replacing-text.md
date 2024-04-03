---
date: 2024-01-20 17:58:13.456278-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) ."
lastmod: '2024-03-13T22:44:52.378955-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

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
