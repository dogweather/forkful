---
title:                "एक डायरेक्टरी मौजूद हैं कि नहीं जांचें"
html_title:           "Kotlin: एक डायरेक्टरी मौजूद हैं कि नहीं जांचें"
simple_title:         "एक डायरेक्टरी मौजूद हैं कि नहीं जांचें"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Kotlin में डायरेक्टरी मौजूद होने की जांच:

## What & Why?

जब हम एक कोडिंग प्रोजेक्ट में काम करते हैं, तो हमें कई बार अपनी प्रोग्राम फ़ाइलों को अन्य फ़ोल्डर में स्थानांतरित करने की ज़रूरत पड़ती है। जब हम इन फ़ोल्डरों को स्थानांतरित करते हैं, तो हमें पहले से ही मौजूद डायरेक्टरी को चेक करना ज़रूरी होता है। कई बार हमें अपनी प्रोग्राम को डायरेक्टरी को चेक करने के लिए अलग से कोड लिखना पड़ता है, जो हमारा समय और प्रयास बर्बाद करता है। इस समस्या को हल करने के लिए Kotlin में डायरेक्टरी को चेक करने के लिए एक आसान तरीका है जो हमारे इस लेख में विस्तार से बताया गया है।

## How to:

```kotlin
val file = File("path/to/directory")
if (file.isDirectory) {
    println("Directory exists")
} else {
    println("Directory does not exist")
}
```

इस कोड में, हमने File क्लास का एक ऑब्जेक्ट बनाया है जिसको हम अपनी डायरेक्टरी के पथ से इनिशिलाइज़ करते हैं। और उसके बाद हमने isDirectory का उपयोग करके डायरेक्टरी की उपस्थिति को चेक किया है। अगर डायरेक्टरी मौजूद है, तो हमें "Directory exists" प्रिंट होगा और अगर डायरेक्टरी मौजूद नहीं है, तो "Directory does not exist" प्रिंट होगा।

## Deep Dive:

इस तरह के चेक को हमें हर बार कई बार लिखने की ज़रूरत नहीं पड़ती है क्योंकि Kotlin में isDirectory फ़ंक्शन का प्रयोग करके डायरेक्टरी की उपस्थिति को आसानी से चेक किया जा सकता है। इस से हमारा समय और प्रयास बचता है और हमारे कोड को स्पष्ट और संक्षिप्त भी बनाता है।

## See Also:

अगर आपको Kotlin में अन्य फ़ाइल और फ़ोल्डर से संबंधित काम करना है, तो आप निम्नलिखित स्रोतों को भी देख सकते हैं:

- [Kotlin Official Documentation: File Handling](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html): Kotlin के आधिकारिक डॉक्यूमेंटेशन जहां फ़ाइल हैंडलिंग से संबंधित सभी जानकारियां उपलब्ध हैं।
- [GeeksforGeeks: File Handling in Kotlin](https://www.geeksforgeeks.org/file-handling-in-kotlin/): Kotlin में फ़ाइल हैंडलिंग को विस्तार से समझाने के लिए यह अच्छा स्रोत है।