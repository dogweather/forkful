---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग कैपिटलाइज़ करना मतलब है हर शब्द के पहले अक्षर को बड़ा (कैपिटल लेटर) करना। प्रोग्रामर इसे डेटा फॉर्मेटिंग, यूजर इंटरफेस को सुधारने, या टेक्स्ट नॉर्मलाइजेशन के लिए करते हैं।

## कैसे करें:

```Haskell
import Data.Char (toUpper)

-- सिम्पल फंक्शन जो पहले अक्षर को बड़ा करता है।
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

-- प्रत्येक शब्द को कैपिटलाइज़ करने वाला फंक्शन।
capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalize . words

main :: IO ()
main = do
    let exampleString = "यह हास्केल उदाहरण है।"
    putStrLn $ capitalizeWords exampleString
```

**Sample Output:**
```
"यह हास्केल उदाहरण है।"
```

## गहराई से जानकारी:

जब हम टेक्स्ट प्रोसेसिंग करते हैं, तो कभी-कभी हमें स्ट्रिंग्स को नाम, शीर्षक या दूसरे डोमेन-विशेष कारणों के लिए स्टैण्डर्डाइज़ करने की जरूरत होती है। हास्केल में `Data.Char` मॉड्यूल `toUpper` फंक्शन के जरिए हमें ये कार्यशीलता प्रदान करता है। इसका विकल्प हो सकता है एक लाइब्रेरी जैसे कि `text` पैकेज जो दक्षता और अधिक कार्यक्षमता के लिए हास्केल में टेक्स्ट प्रोसेसिंग में सहायक होती है।

## यह भी देखें:

- Haskell `Data.Char` Module Documentation: [Hackage Data.Char](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
- Haskell Text Processing with the `text` Package: [Hackage Text](https://hackage.haskell.org/package/text)
- "Learn You a Haskell for Great Good!" कैपिटलाइज़ेशन संबंधित अध्याय: [Learn You a Haskell](http://learnyouahaskell.com/)

**नोट:** आउटपुट में बदलाव हो सकता है यदि हास्केल संस्करण या पर्यावरण विभिन्न हो।
