---
title:                "YAML से काम करना"
html_title:           "Haskell: YAML से काम करना"
simple_title:         "YAML से काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों लोग इसे करते हैं?
YAML काम करने का एक तरीका है जिससे आप अपने डेटा को एक आसान और स्ट्रक्टर्ड फॉर्म में संग्रहीत कर सकते हैं। यह एक plaintext फाइल है जिसे आप आसानी से पार्स और रीड कर सकते हैं। यह इस्तेमाल करने में बहुत सरल है और यह आपके कोड को अत्यंत पारदर्शी बनाता है।

## कैसे करें:
आप Haskell में YAML फ़ाइलों से डेटा पार्स कर सकते हैं और JSON या अन्य फॉर्मेट में डेटा बना सकते हैं। नीचे दिए गए उदाहरण में, हम पांच नए उदाहरण दिखाएंगे। आप इन्हें अपने खुद के कोड में उपयोग करके सीख सकते हैं।

```Haskell
-- पार्सिंग YAML फ़ाइल से डेटा
import Text.Libyaml

main = do
    str <- parseYamlFile "example.yaml"
    -- str अब एक Extended Data Type है, जो आपको संपूर्ण YAML फ़ाइल के डेटा को पार्स करके देगा

-- नए YAML फ़ाइल बनाना
import Text.Libyaml

main = do
    let data = [("name", "Alice"), ("age", "25"), ("city", "New York")]
    let yamlStr = toYaml data
    writeFile "new.yaml" yamlStr
    -- new.yaml फ़ाइल अब आपके नए डेटा से बन गई है

-- बूलियन वैल्यू से YAML फ़ाइल बनाना
main = do
    let data = [("isStudent", True), ("hasJob", False)]
    let yamlStr = encodeYaml data
    putStrLn yamlStr
    -- isStudent: true
    -- hasJob: false

-- संख्याओं वाली YAML फ़ाइल बनाना
main = do
    let data = [("price", 19.99), ("quantity", 10)]
    let yamlStr = encodeYaml data
    putStrLn yamlStr
    -- price: 19.99
    -- quantity: 10

-- मल्टीलाइन टेक्स्ट वाली YAML फ़ाइल बनाना
main = do
    let data = [("description", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque at nisi euismod, feugiat arcu et, porta risus. Donec gravida nibh vel erat dictum, non.</br>")]
    let yamlStr = encodeYaml data
    putStrLn yamlStr
    -- description: |
    --     Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque at nisi euismod, feugiat arcu et, porta risus. Donec gravida nibh vel erat dictum, non.

```

## गहराई में जाइए:
YAML का इतिहास 2001 में Clark Evans और Ingy döt Net द्वारा जन्म लिया गया था। इससे पहले, programmers ने अपने डेटा को संग्रहीत करने के लिए XML या JSON जैसे अन्य formats का उपयोग किया था। YAML उन सभी को आसान होने के साथ-साथ एक readable format भी प्रदान करता है। कुछ अन्य alternatives के रूप में TOML और Ini फ़ाइलें भी हैं।

YAML आमतौर पर इंडेंटेड होता है, यानि स्पेसकोईंग का इस्तेमाल करके visual ही दिखाई देता है। यह आवश्यकतानुसार multiline और सिंगललाइन वैल्यूज़ दोनों का समर्थन करता है। YAML एक बहुत ही लाइटवेट और सरल syntax है, जो आपको अपने कोड को अधिक maintain करन