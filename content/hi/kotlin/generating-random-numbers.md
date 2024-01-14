---
title:    "Kotlin: रैन्डम नंबर्स उत्पन्न करना"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## क्यों

यदि आप किसी भी programming language में काम करते हैं, तो आपने शायद random numbers का इस्तेमाल करने के बारे में सुना होगा। Random numbers को generate करना एक बहुत ही उपयोगी कौशल है जो आपको अपनी coding skills को निखारने में मदद कर सकता है।

## कैसे करें

 ```Kotlin
fun main() {
    // Random numbers generate करने के लिए, Random() का इस्तेमाल करें। 
    val randomNumber = Random()
    // NextInt() function का इस्तेमाल करके, हम एक random integer output प्राप्त करेंगे।  
    val output = randomNumber.nextInt()
    println(output) // कोड के अनुसार हमें अलग-अलग random integer output प्राप्त होगा।
}
```

 ```Kotlin
fun main() {
    // NextInt() के साथ, हम range भी specify कर सकते हैं। जैसे, हम 1 से 10 तक के numbers में से एक random number generate कर सकते हैं। 
    val randomNumber = Random()
    val output = randomNumber.nextInt(10) + 1
    println(output) // कोड के अनुसार हमें 1 से 10 तक के numbers में से एक random number output प्राप्त होगा।
}
```

## गहराई में (Deep Dive)

Random numbers generate करने की प्रक्रिया काफी complex हो सकती है। हालांकि, आपको इसे समझने के लिए एक क्षमता की आवश्यकता नहीं है। लेकिन अगर आप चाहें, तो जान सकते हैं कि random numbers कैसे generate किए जाते हैं।

मूलतः, random numbers के generate करने के लिए, algorithms का इस्तेमाल किया जाता है जो आपको एक unpredictable sequence देंगे। इससे, ये numbers कैसे generate किए जाते हैं, यहां हमारे लिए एक महत्वपूर्ण बात नहीं होती है। हमें बस इस बात का महत्व है कि हमें एक उपयोगी random output मिल जाना चाहिए।

## देखें भी

- [Random number generation in Java](https://www.geeksforgeeks.org/random-number-generator-in-java/)
- [Generating random numbers in C++](https://www.cplusplus.com/reference/random/)
- [Using Random class in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)