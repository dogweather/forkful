---
title:    "Kotlin: उपस्थापित उपस्थितियों को निकालना"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## क्यों

कोटलिन में स्ट्रिंग्स से उपस्थिति निकालने के लिए आपको एक विशेष स्थिति या समस्या को हल करने की आवश्यकता हो सकती है। इसके लिए आपको स्ट्रिंग्स के सहायता से उपस्थिति को निकालना हो सकता है।

## कैसे करें

`` `कोटलिन 
// सभी निर्णायक उत्पाद
val कोड = "Kotlin सिखना मजेदार और उपयोगी है।"
// सबसे पहला शब्द उपस्थिति निकालना
val शब्द = कोड.उपस्थिति ("Kotlin")
// परिणाम छापें
println (word) // उत्पाद: Kotlin
`` `

## गहराई में नैसर्गिक

स्ट्रिंग्स से उपस्थिति निकालने के लिए कोटलिन में `उपस्थिति ()` फ़ंक्शन का उपयोग किया जाता है। यह फ़ंक्शन स्ट्रिंग पैरामीटर को दूसरे स्ट्रिंग से खोजता है और पहली से मिलाता है।

## देखें भी

- [कोटलिन बेसिक्स ट्यूटोरियल] (https://www.geeksforgeeks.org/introduction-to-kotlin/) 
- [कोटलिन स्ट्रिंग्स से उपस्थिति हटाएं] (https://www.codegrepper.com/code-examples/kotlin/kotlin+replace+all+occurrences+of+string) 
- [कोटलिन स्ट्रिंग्स के लिए विशेषक बनाएं] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)