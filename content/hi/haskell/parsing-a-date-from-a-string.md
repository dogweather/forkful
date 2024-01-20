---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग से डेट का पार्स करना एक प्रक्रिया है, जिसमें हम टेक्स्ट रूप में दी गई तारीख को कंप्यूटर के समझने योग्य तारीख फॉर्मॅट में बदलते हैं। इसे प्रोग्रामर्स डेट से संबंधित ऑपरेशन्स और समन्वय (सिंख्रोनाइज़ेशन) को सीधे करने के लिए करते हैं।

## कैसे करें:

Haskell में, हम पैकेज `Data.Time` का उपयोग करके यह कर सकते हैं। यहां कुछ कोड के उदाहरण और आउटपुट हैं:

```Haskell
import Data.Time

parseDate :: String -> Day
parseDate input = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" input
```

उदाहरण के लिए:
```Haskell
parseDate "2021-09-15"
```

आउटपुट:
```Haskell
2021-09-15
```

## गहरा डाइव:

1. **ऐतिहासिक संदर्भ**: Haskell के पुराने संस्करणों में, `Data.Time` पैकेज मौजूद नहीं था। इसके बजाय, `time` पैकेज का उपयोग किया जाता था।
2. **विकल्प**: `thyme` और `time` जैसे कुछ अन्य पैकेज्स भी उपलब्ध हैं, जो डेट पार्सिंग का समर्थन करते हैं।
3. **आधार विवरण**: `parseTimeOrError` फ़ंक्शन तारीख की स्ट्रिंग को पार्स करता है, रूप रेखा (मार्कअप) निर्दिष्ट करने के लिए एक फ़ॉर्मेट स्ट्रिंग को स्थानांक की तरह उपयोग करता है। यदि पारिंग विफल होती है, तो इसे एक त्रुटि उत्पन्न करता है।

## देखें भी:

व्या पार्सिंग के सम्बंध में अधिक जानकारी के लिए, निम्नलिखित स्रोतों की जांच करें:

1. [Haskell time package](https://hackage.haskell.org/package/time)