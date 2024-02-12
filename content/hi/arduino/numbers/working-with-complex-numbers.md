---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-01-26T04:37:55.948516-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएँ एक वास्तविक भाग और एक काल्पनिक भाग से मिलती हैं, जिन्हें आमतौर पर `a + bi` के रूप में लिखा जाता है। वे सिग्नल प्रोसेसिंग, इलेक्ट्रिकल इंजीनियरिंग, या किसी अन्य क्षेत्र जहाँ घटनाओं को एक प्लेन में सबसे अच्छा मॉडल किया जाता है, में शामिल कुछ गणित-गहन Arduino प्रोजेक्ट्स के लिए अत्यंत महत्वपूर्ण हैं।

## कैसे करें:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // सीरियल संचार शुरू करें
  
  Complex myComplex(2, 3); // एक जटिल संख्या 2 + 3i बनाएं
  Complex anotherComplex(1, 1); // एक और जटिल संख्या 1 + 1i बनाएं
  
  // जोड़
  Complex result = myComplex + anotherComplex; 
  Serial.print("जोड़: "); 
  result.print(); // आउटपुट 3 + 4i
  
  // गुणा
  result = myComplex * anotherComplex; 
  Serial.print("गुणा: ");
  result.print(); // आउटपुट -1 + 5i
}

void loop() {
  // इस उदाहरण में इस्तेमाल नहीं किया गया
}
```
नमूना आउटपुट:
```
जोड़: 3 + 4i
गुणा: -1 + 5i
```

## गहराई में समझें
मूल रूप से, जटिल संख्याओं को संदेह के साथ मिला था, लेकिन वे विभिन्न वैज्ञानिक क्षेत्रों में केंद्रीय बन गए हैं। ऐतिहासिक रूप से, उन्हें उन पॉलीनोमियल समीकरणों के समाधान प्रदान करने के लिए मान्यता प्राप्त थी जिनके वास्तविक समाधान नहीं होते हैं।

Arduino अपनी मानक लाइब्रेरी में जटिल संख्याओं को शामिल नहीं करता है, लेकिन आप `Complex.h` जैसी लाइब्रेरीज का उपयोग करके उन्हें संभाल सकते हैं। आंतरिक रूप से, ये लाइब्रेरीज एक Complex क्लास को परिभाषित करती हैं, आमतौर पर दो डबल्स का उपयोग करके वास्तविक और काल्पनिक भागों को स्टोर करते हैं, और अंकगणित का समर्थन करने के लिए ऑपरेटरों को ओवरलोड करते हैं।

वैकल्पिक रूप से, जो अनुप्रयोग जटिल संख्या अंकगणित को मूल रूप से आवश्यक नहीं मानते हैं, वे अन्य गणितीय रणनीतियों या लाइब्रेरीज का उपयोग कर सकते हैं। हालाँकि, याद रखें कि जटिल संख्याओं के बजाय फ्लोट्स का उपयोग कुछ समस्याओं को बहुत सरल बना सकता है।

## देखें भी
- रॉब टिल्लार्ट द्वारा [Complex.h](https://github.com/RobTillaart/Complex) लाइब्रेरी।
- [जटिल संख्याओं के पीछे की गणित](https://mathworld.wolfram.com/ComplexNumber.html) में गहराई से डाइव।