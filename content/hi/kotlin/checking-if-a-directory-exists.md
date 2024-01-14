---
title:    "Kotlin: डायरेक्ट्री मौजूद है या नहीं जांचना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#### क्यों

डायरेक्टरी का अस्तित्व पता करने से पहले कोई उपयोगकर्ता अपने ऐप्लिकेशन में एक निश्चित डायरेक्टरी में नए फ़ाइलें या फ़ोल्डर बनाने के लिए पहले से ही मौजूद न होने की स्थिति में यह उपयोगी हो सकता है।

## कैसे

जब हमें कोई ऐप्लिकेशन डायरेक्टरी का अस्तित्व जानना होता है। तो हम `exists()` फ़ंक्शन का इस्तेमाल कर सकते हैं। इस फ़ंक्शन को `File` क्लास में पाया जाता है। यह तरीका सबसे आसान है।

```Kotlin
val directory = File("/Users/john/blog") 
val exists = directory.exists()
println("डायरेक्टरी अस्तित्व होने पर: $exists")
```

जब अस्तित्व होता है तो यह आउटपुट दिखाई देगा:

```
डा