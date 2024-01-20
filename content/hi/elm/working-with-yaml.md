---
title:                "Yaml के साथ काम करना"
html_title:           "Elm: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या है और क्यों करें? 
YAML काम करना होता है तथा उसका उपयोग क्यों किया जाता है।

YAML को दस्तावेज़ या डेटा को संरचित और सामान्य ढंग से प्रकट करने के लिए उपयोग किया जाता है। कारण इससे डेटा को प्रवर्तन करने में आसानी होती है।

## कैसे करें: 
एल्म में YAML के साथ काम करने के लिए, आप निम्न प्रकार से कोड कर सकते हैं:

```Elm
import Yaml exposing (..)

-- पाठ को YAML में संगठित करें
sampleData =
  "name: जॉन डो र"
  "उम्र: २५"
  "शहर: न्यूयॉर्क"

-- YAML से पाठ को पढ़ें
parsedData = Yaml.decode sampleData

-- प्रदर्शित करें
case parsedData of
  Ok data ->
    Debug.log "पाठ" data

  Err err ->
    Debug.log "त्रुटि" err
```

आपको निम्न अंकों की सूची मिल सकती है:

- पाठ से YAML को प्रस्तुत किया जा सकता है
- YAML से प्रविष्ट पाठ को पढ़ा जा सकता है
- Yaml.decode फ़ंक्शन का उपयोग करने से संरचित पाठ को ट्री में विन्यस्त किया जा सकता है


## गहराई से:
YAML का निर्माण काम करना सहमतियों को विकसित XML को स्थानांतरित करने के लिए चित्रित किया गया था। इससे स्पष्ट है कि यह प्रतिस्थापन के लिए संघर्ष दुष्प्रभावों को बढ़ा सकता है, परंतु यह कुछ उचित गुण वैसे न हो तो किसी के कम होंगे।

अधिक जानकारी के लिए निम्नलिखित स्त्रोत की जाँच करें:

- [YAML के मूल विधि नाम --- डेटासहिता](https://yaml.org/spec/history/2001-12-10.html)

## देखें भी: