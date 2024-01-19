---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

स्ट्रिंग इंटरपोलेशन के माध्यम से, हम एक स्ट्रिंग में डायरेक्टली हास्केल एक्सप्रेशन्स इनकॉर्पोरेट कर सकते हैं। यह एकदिवसीय और लचीला मार्ग होता है जो इनपुट परिवर्तनों को संचालित करने के लिए प्रोग्रामर्स का उपयोग करते हैं।

## कैसे करें:

यहाँ एक उदाहरण है:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.String.Interpolate

printInterpolated :: String -> String -> IO ()
printInterpolated name server = putStrLn [i|Hello, #{name}! Connecting to #{server}.|]

main :: IO ()
main = do
  printInterpolated "Alice" "Server 1"
```

यह उद्धरण निम्नलिखित आउटपुट पैदा करेगा:

```
Hello, Alice! Connecting to Server 1.
```

## गहरी डाइव:

**ऐतिहासिक संदर्भ:** 
हास्केल की एक + + ऑपरेटर ने पहले स्ट्रिंग इंटरपोलेशन का समर्थन किया था। लेकिन, इस समय "Data.String.Interpolate" लाइब्रेरी सबसे अधिक विश्वसनीय और प्रयोग की गई है।

**विकल्प:** 
आप भी स्ट्रिंग इंटरपोलेशन को `"++"` ऑपरेटर के साथ फॉर्मेट कर सकते हैं। यह एक विचलन या एक्सप्रेशन का फॉर्मेट करने के लिए उपयोग किया जा सकता है।

**कार्यान्वयन विवरण:**
हास्केल में स्ट्रिंग इंटरपोलेशन "QuasiQuotes" और "Data.String.Interpolate" लाइब्रेरी का उपयोग करके किया जाता है। 
QuasiQuotes (क़्वासीकोट्स) हास्केल कोड के एक हिस्से को आपके स्ट्रिंग में एंबेड करने का एक तरीका हैं। 

## अधिक देखें:

हास्केल स्ट्रिंग इंटरपोलेशन के बारे में अधिक जानकारी के लिए, आप निम्नलिखित संसाधनों की जांच कर सकते हैं:

1. Hackage पर "Data.String.Interpolate" लाइब्रेरी: https://hackage.haskell.org/package/interpolate
2. Haskell Wiki पर क्वासिकोटेशन:  https://wiki.haskell.org/Quasiquotation
3. Haskell Wiki पर ओवरलोडेड स्ट्रिंग्स: https://wiki.haskell.org/Overloaded_strings