---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Kotlin: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कंकटेंट स्ट्रिंग क्या है और क्यों प्रोग्रामर्स इसे करते हैं, इसके बारे में दो से तीन सेंटेंसों में समझाया जाएगा।

कंकटेंटिंग स्ट्रिंग सिर्फ दो या अधिक स्ट्रिंग को एक साथ जोड़ने की प्रक्रिया है। यह प्रोग्रामर्स द्वारा उपयोग किया जाता है क्योंकि यह स्ट्रिंग्स को संपादित, साफ़ और जोड़ने में आसान बनाता है।

## कैसे करें?
```Kotlin
var greeting = "नमस्ते"
var name = "अनुष्का"
var message = greeting + " " + name
println(message)
```
यह कोड आउटपुट में नमस्ते अनुष्का दिखाएगा।

```Kotlin
var number1 = 10
var number2 = 5
var sum = "The sum of " + number1 + " and " + number2 + " is " + (number1 + number2)
println(sum)
```
यह कोड आउटपुट में The sum of 10 and 5 is 15 दिखाएगा।

## गहराई तक जाइए:
कंकटेंटिंग स्ट्रिंग को निर्देशिका या डेटा संरचनाओं को मूलभूत स्थिति से जोड़ने के लिए बनाया गया था। इसके अलावा, विभिन्न तरीकों से स्ट्रिंग को जोड़ा जा सकता है जैसे + और plus () फ़ंक्शन। कोट्स को जोड़ने के लिए, आपको इसे छोटी या बड़ीतर गोलियों में चारों ओर रखने की आवश्यकता हो सकती है।

## अधिक जानें:
कंकतेंटिग स्ट्रिंग के अलावा, कोट्स को जोड़ने के लिए आपके पास अन्य विकल्प हैं - String.format () और StringBuilder। इनमें से String.format () सेकेंडरी स्ट्रिंग्स और लूप का उपयोग करने की आवश्यकता को कम करता है। StringBuilder फ़ंक्शन स्ट्रिंग को मूलत: मूलभूत स्थिति से जोड़ता है।

## अधिक जानकारी के लिए:
कंकटेंटिंग स्ट्रिंग को समझने और उसका उपयोग करने में कोई दिक्कत नहीं है। इसके अलावा, आप [कोट्लिन ऑफिशियल डॉक्यूमेंटेशन] (https://kotlinlang.org/docs/reference/basic-types.html#strings) को देख सकते हैं।

## अन्य स्रोत:
- स्ट्रिंग के साथ विभिन्न काम करने के युक्तियाँ https://www.baeldung.com/kotlin/string-operations
- String format कैसे करें https://www.woolha.com/tutorials/kotlin-string-format-examples-formatting-a-string-in-kotlin
- String.format () के बारे में अधिक जानने के लिए https://www.jetbrains.com/help/idea/using-string-format-method.html