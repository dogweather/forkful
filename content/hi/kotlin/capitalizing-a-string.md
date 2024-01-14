---
title:                "Kotlin: स्ट्रिंग को पूँजीकरण देना"
simple_title:         "स्ट्रिंग को पूँजीकरण देना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

हेलो दोस्तों! स्वागत है आपका हमारे प्रोग्रामिंग ब्लॉग पोस्ट पर। अगर आप कोतलिन का उपयोग करते हैं और उसमें नए हैं, तो आज का लेख आपके लिए बहुत ही महत्वपूर्ण हो सकता है। हम आज कोतलिन में गिनी जानी वाली एक महत्वपूर्ण टेक्निक के बारे में बात करेंगे - उसके नए हाशिये करने का तरीका।

## क्यों?

अक्सर हमें एक स्ट्रिंग के पहले अक्षर को बड़ा करने की आवश्यकता होती है। इसे कैपिटलाइज करना कहते हैं। उदाहरण के तौर पर, "hello" शब्द को कैपिटलाइज करने से "Hello" शब्द बन जाता है। इसे करने से हमारे प्रोग्राम में एक अलग तरह का छाप दिखाई देता है और स्ट्रिंग को पढ़ने में भी आसानी होती है।

## कैसे करें?

कोतलिन में इस काम को करने के लिए हमारे पास बहुत से विकल्प हैं। प्रथम अनुमान हम इसे परमेटरिज्ड कंस्ट्रक्टर्स का प्रयोग कर सकतें हैं। इसमें हम एक स्ट्रिंग पास करते हैं जो कैपिटलाइज करना है।

```Kotlin
var string = "hello"
var capitalizedString = string.capitalize()
println(capitalizedString)

// Output: Hello
```

दूसरा तरीका हम यूज कर सकते हैं वह है String Templates का। इसमें हम $ के इस्तेमाल से अपनी स्ट्रिंग को कैपिटलाइज कर सकते हैं।

```Kotlin
var string = "hello"
var capitalizedString = "${string.capitalize()}"
println(capitalizedString)

// Output: Hello
```

हम ये काम भी एक सदिया तरीके से कर सकते हैं। एक सदिया अपरेटर्स का प्रयोग करके।

```Kotlin
var string = "hello"
var capitalizedString = string.first().toUpperCase() + string.substring(1)

// Output: Hello
```

## गहर