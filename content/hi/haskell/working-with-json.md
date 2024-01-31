---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON यानी JavaScript Object Notation, एक आम डेटा-फॉर्मेट है, जिसका इस्तेमाल सर्वर और वेब एप्लीकेशंस के बीच डेटा एक्सचेंज के लिए होता है. प्रोग्रामर JSON का इस्तेमाल इसलिए करते हैं क्योंकि यह हल्का, समझने में आसान, और भाषा-निरपेक्ष है.

## कैसे करें:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- JSON डेटा का एक उदाहरण
jsonInput = "{\"name\": \"Ajay\", \"age\": 30}"

-- हमारे Haskell ऑब्जेक्ट के लिए एक डेटा टाइप
data Person = Person { name :: String, age :: Int } deriving Show

-- JSON से Haskell टाइप में कन्वर्ट करने के लिए instance बना रहे हैं
instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
      <$> v .: "name"
      <*> v .: "age"

-- JSON को पार्स करने का फंक्शन
parseJson :: B.ByteString -> Maybe Person
parseJson jsonData = decode jsonData

-- मेन फंक्शन
main :: IO ()
main = do
  let decoded = parseJson (B.pack jsonInput)
  print decoded
```

सैंपल आउटपुट:
```
Just (Person {name = "Ajay", age = 30})
```

## गहराई में जानकारी:

JSON, 2001 से डेटा फॉर्मेट के रूप में इस्तेमाल हो रहा है और Douglas Crockford द्वारा पॉपुलर बनाया गया. इसके विकल्प के रूप में XML और YAML जैसे फॉर्मेट्स भी हैं, पर JSON इनसे अधिक संक्षिप्त और तेज है. Haskell में JSON पर काम के लिए `aeson` पैकेज का इस्तेमाल किया जाता है, जो अन्न्य प्रोग्रामिंग भाषाओं के JSON लाइब्रेरीज़ की तरह ही काम करता है.

## इसे भी देखें:

- JSON के अधिक जानकारी के लिए: [JSON.org](http://json.org/)
- `aeson` पैकेज डॉक्युमेंटेशन: [Aeson Hackage](https://hackage.haskell.org/package/aeson)
- Haskell `aeson` ट्यूटोरियल: [Aeson Tutorial](https://artyom.me/aeson)
