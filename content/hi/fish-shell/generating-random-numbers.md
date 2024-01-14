---
title:                "Fish Shell: तस्वीरें उत्पन्न करना"
simple_title:         "तस्वीरें उत्पन्न करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## इसका कारण

डेटा विज्ञान, मशीन लर्निंग और साइबर सुरक्षा जैसे क्षेत्रों में सत्यापित और उत्पन्न डेटा के लिए यादृच्छिक संख्याओं का उपयोग करना जरूरी है। यह डेटा को संरचित करने और उससे सार्वजनिक नमूनों को अधिक संवेदनशील बनाने में मदद करता है, जो अनुसंधान और विश्लेषण के लिए अनुकूल होते हैं।

## कैसे करे

फिश शेल में, हम `math/rand` लाइब्रेरी का उपयोग करके `rand` कमांड को चला सकते हैं। उसे उदाहरण के साथ बिंदु निर्देशित करें और क्रम में चालू करें।

```fish
set min 1    # Minimum value
set max 100  # Maximum value
set count 10 # Number of random numbers to generate
for i in (seq $count)
    echo (rand -r $min $max)
end
```

प्रत्येक दौर में, यह कमांड रैंडमाइज़ड अंक लायेगा जो `min` और `max` के बीच होते हैं। उपरोक्त कोड और संबंधित विषयों पर और अधिक अध्ययन कर सकते हैं।

## गहराई में जाए

हम डेटा भण्डारण में कहाँ, डेटा विश्लेषण में कितना समय व्यतीत होता है, और क्या उसका प्रभाव हो सकता है और अन्य बातें पर भी प्रभावित होते हैं। अतिरिक्त विवरण के लिए माइकल कोड्डन का [यह लेख] (https://www.extraordinaryproblems.com/understanding-generating-random-numbers/) देखें।

## यहाँ देखें

[फिश शेल ऑफिशियल दस्तावेज़ीकरण] (https://fishshell.com/docs/current/) | [मजबूतियाँ और मुश्किलें] (https://fishshell.com/docs/current/index.html#strongfeatures) | [कमांड लाइन मानचित्र] (https://learnxinyminutes.com/docs/fish/) | [मशीन लर्निंग और