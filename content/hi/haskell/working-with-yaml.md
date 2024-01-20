---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML यानी "YAML Ain't Markup Language" एक सरल डेटा सीरियलाइज़ेशन फॉर्मेट है जिसका इस्तेमाल कॉन्फ़िगरेशन फ़ाइल, डेटा आदान-प्रदान, और स्टोरेज के लिए होता है। प्रोग्रामर YAML का उपयोग इसकी सरलता और पढ़ने में आसानी के कारण करते हैं।

## कैसे करें:

Haskell में YAML से काम करने के लिए `yaml` पैकेज का उपयोग होता है। सबसे पहले, Stack या Cabal का इस्तेमाल कर `yaml` पैकेज इंस्टॉल करें। 

```Haskell
-- dependencies: yaml
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

-- YAML स्ट्रिंग से पार्स करने का फंक्शन
parseYAML :: BS.ByteString -> IO ()
parseYAML yamlData = do
    let parsed = decodeEither' yamlData :: Either ParseException Value
    case parsed of
        Left err -> print err
        Right val -> print val

main :: IO ()
main = do
    let myYAML = "name: John Doe\nage: 30\n"
    parseYAML (BS.pack myYAML)
```

इस कोड में, हमने एक YAML स्ट्रिंग `myYAML` से पार्स किया है और पार्स किए हुए डेटा को प्रिंट किया है।

## गहराई से समझिए:

YAML की शुरुआत 2001 में हुई थी और यह JSON का एक विकल्प है क्योंकि यह ह्यूमन-रीडेबल है। YAML में डेटा स्ट्रक्चर को समझना आसान होता है, और इसे टैब्स के बजाय इंडेंटेशन से रिप्रेजेंट किया जाता है। Haskell में `yaml` पैकेज लाइब्रिएरी `libyaml` पर आधारित है और `aeson` पैकेज के डेटा टाइप्स के साथ इंटीग्रेशन प्रदान करता है जो JSON प्रोसेसिंग के लिए भी इस्तेमाल होता है। 

JSON की तरह, YAML भी प्लेटफार्म-निरपेक्ष है और विभिन्न प्रोग्रामिंग भाषाओं में पार्स और जेनरेट किया जा सकता है, लेकिन इसे पढ़ना और समझना आसान होता है, जिससे यह कॉन्फ़िगुरेशन फ़ाइलों के लिए ज्यादा प्रिय है।

## और भी स्रोत:

- YAML स्पेसिफिकेशन: [The Official YAML Website](https://yaml.org/)
- Haskell `yaml` पैकेज: [yaml on Hackage](https://hackage.haskell.org/package/yaml)
- Haskell JSON प्रोसेसिंग (`aeson` पैकेज): [aeson on Hackage](https://hackage.haskell.org/package/aeson)
- YAML और JSON की तुलना: [Comparing YAML and JSON](https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON)