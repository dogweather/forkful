---
title:                "Kotlin: आमतौर प्रयोग करना"
simple_title:         "आमतौर प्रयोग करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

# हम क्यों इस्तेमाल करे जनरल एक्सप्रेशन्स?
जनरल एक्सप्रेशन्स के उपयोग से हम टेक्स्ट स्ट्रिंग में सरलता से कुछ टेक्स्ट ढूंढ़ सकते हैं, जो हमारे लिए जरूरी हो सकता है। यह टेक्स्ट स्ट्रिंग में एक या अधिक शब्दों, पैटर्न या डिजिटस की खोज करने में हमें मदद कर सकता है।

## कैसे करें
जनरल एक्सप्रेशन्स का उपयोग करना बहुत सरल है। सबसे पहले, हम `Regex` ऑब्जेक्ट बनाते हैं और उसमें हमारे कॉम्पाइल्ड पैटर्न डालते हैं। फिर हम इसे `find()` या `matchEntire()` के साथ `String` पर कॉल करते हैं। नीचे दिए गए उदाहरण में, हमने दो शब्दों के बीच कुछ टेक्स्ट स्ट्रिंग से शुरू होने वाले शब्दों की खोज की है।

```Kotlin
val pattern = Regex("hello")
val text = "hey there, hello world!"
val result = pattern.find(text)
println(result?.value)
```

उपरोक्त कोड का आउटपुट `hello` होगा। हमारे द्वारा निर्दिष्ट पैटर्न के अनुसार, यह सबसे पहला `hello` होगा। अगर आप शुरू में ही दोनों शब्दों के बीच कोई और शब्द खोजना चाहते हैं, तो आप `matchEntire()` का उपयोग कर सकते हैं जो स्ट्रिंग की पूरी मिलान करेगा।

```Kotlin
val pattern = Regex("^hello ")
val text = "hello world, hey there, hello universe!"
val result = pattern.matchEntire(text)
println(result?.value)
```

उपरोक्त कोड का आउटपुट `hello world` होगा। हमने उस स्ट्रिंग को खोजा है जो `hello` से शुरू होता है और अपने बाद कुछ भी आ सकता है। इस उदाहरण में, हमारे बनाये गए कॉम्पाइल्ड प