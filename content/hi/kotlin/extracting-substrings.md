---
title:                "उपस्तर समूहों को निकालना"
html_title:           "Kotlin: उपस्तर समूहों को निकालना"
simple_title:         "उपस्तर समूहों को निकालना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
"Substring निकालना" क्या है, इसका प्रयोग क्यों करते हैं हम प्रोग्रामर? इसका सरल जवाब यह है कि substring उपस्थित string से छोटा string होता है जो उपस्थित string के कुछ अंशों को अलग कर लेता है। प्रोग्रामर इसका उपयोग स्ट्रिंग का स्नेहपूर्ण डेटा प्रकार के रूप में करते हैं।

## कैसे करें:
```Kotlin
val str = "Hello World"
val subStr = str.substring(0, 5)
println(subStr)
```

आपको बस मूल स्ट्रिंग(str) से substring(subStr) की शुरुआत और समाप्ति के स्थान को निर्दिष्ट करना होगा। यहां हमने "Hello World" स्ट्रिंग का प्रथम पांच अक्षर substring के रूप में निकाला है। यह कमान "Hello" स्क्रीन पर प्रिंट करेगी।

## गहराई में जाएँ:
substring का इतिहास दोनों 1957 में और 1966 में IBM ने प्रकाशित किया गया। substring डेटा को उपयुक्त ढंग से काटने के लिए कई तकनीकों और अल्गोरिदमों का उपयोग करता है। इसके अलावा, प्रोग्रामरों द्वारा substring का उपयोग स्ट्रिंग का अधिक मात्रा में डेटा प्राप्त करने के लिए भी किया जाता है।

## इसके अतिरिक्त देखें:
- [Kotlin Official Documentation on Substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [TutorialsPoint: Kotlin - Substring](https://www.tutorialspoint.com/kotlin/kotlin_substring.htm)
- [Java2s: Kotlin substring() function with example](http://www.java2s.com/Tutorials/Kotlin/Kotlin_Tutorial/Kotlin_Substring_Function.htm)