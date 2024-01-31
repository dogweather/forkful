---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट फ़ाइल लिखना है डेटा को फ़ाइल में सेव करना। प्रोग्रामर्स इसे कॉन्फिगरेशन, डेटा स्टोरेज, लॉगिंग आदि के लिए करते हैं।

## How to: (कैसे करें:)

### टेक्स्ट फ़ाइल बनाना और डेटा लिखना:
```Haskell
import System.IO

main :: IO ()
main = do
    writeFile "sample.txt" "Haskell में टेक्स्ट फ़ाइल लिखना आसान है।"
    putStrLn "फ़ाइल लिखी गई।"
```
उदाहरण आउटपुट:
```
फ़ाइल लिखी गई।
```

### फ़ाइल में डेटा जोड़ना (append):
```Haskell
main = do
    appendFile "sample.txt" "\nयह एक नई लाइन है।"
    putStrLn "फ़ाइल में जोड़ा गया।"
```
उदाहरण आउटपुट:
```
फ़ाइल में जोड़ा गया।
```

## Deep Dive (गहराई से जानकारी):

टेक्स्ट फ़ाइल लिखने के लिए `writeFile` और `appendFile` फंक्शन `System.IO` मॉड्यूल में पाए जाते हैं। ऐतिहासिक रूप से, Haskell में फाइल I/O कई अब्सट्रैक्शंस के माध्यम से संभव हुआ है, जैसे कि `Handle` आधारित I/O, `lazy I/O` और आधुनिक `conduit` या `pipes` पैकेज। हालांकि, `writeFile` और `appendFile` सीधे और सहज हैं और छोटी फाइल्स के लिए उपयोगी हैं। पुराने विकल्प के रूप में `openFile` के साथ काम किया जा सकता है।

## See Also (और जानकारी के लिए):

- Learn You a Haskell for Great Good! - An accessible guide to Haskell: [Learn You a Haskell](http://learnyouahaskell.com/)
- Real World Haskell - A resource for practical Haskell programming: [Real World Haskell](http://book.realworldhaskell.org/)

इन स्रोतों से आप Haskell में और अधिक फ़ाइल I/O के बारे में जान सकते हैं और अपनी Haskell प्रोग्रामिंग कौशल को सुधार सकते हैं।
