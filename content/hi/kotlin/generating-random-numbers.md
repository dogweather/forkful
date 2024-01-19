---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

यादृच्छिक संख्याओं का उत्पन्न करना मतलब है कि किसी अनुप्रेक्ष्य अवलोकन में अन्य संख्याओं से अधिक प्रमाणिकता प्राप्त करना। यह सॉफ़्टवेयर डेवलपर्स के लिए महत्वपूर्ण है क्योंकि आवश्यकतानुसार अद्वितीय संख्याओं की पैदावार की आवश्यकता हो सकती है, जैसे कि पासवर्ड, OTP आदि।

## कैसे:

Kotlin में, आप `Random()` क्लास का उपयोग करके यादृच्छिक संख्याएं उत्पन्न कर सकते हैं। 

```Kotlin
import kotlin.random.Random

fun main(){
  val randomNo = Random.nextInt(10)
  println(randomNo)
}
```

ऊपरी कोड का चलाने पर आपको 0-9 के बीच का यादृच्छिक संख्या मिलेगा। 

## गहराई में:

(1) ऐतिहासिक संदर्भ: मैथ्स में, यादृच्छिक संख्याओं की अवधारणा काे संगणक विज्ञान में लागू करने का विचार 20 वीं सदी के मध्य में आया। 

(2) विकल्प: यादृच्छिक संख्या उत्पादन थर्ड पार्टी लाइब्रेरी, जैसे कि Apache Commons Math, द्वारा भी किया जा सकता है। 

(3) कार्यान्वयन विवरण: Kotlin में `Random()` क्लास का उपयोग करते समय, इसे अंतर्निहित रूप से Java के `java.util.Random` 클래스 का उपयोग करके यादृच्छिक संख्याएँ उत्पन्न की जाती हैं।

## यह भी देखें:

अधिक जानकारी के लिए निम्नलिखित स्रोतों का उपयोग करें:
1. [जावा उत्कृष्ट प्रयोग (हिंदी)](https://www.baeldung.com/java-random)
2. [कोटलिन डॉक्स](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
3. [Apache Commons Math](http://commons.apache.org/proper/commons-math/)