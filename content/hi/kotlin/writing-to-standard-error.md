---
title:    "Kotlin: स्टैंडर्ड त्रुटि पर लिखना"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## क्यों

क्या आप भी कभी सोचते हैं कि आपकी कोडिंग में कुछ गड़बड़ है और आप उसे ठीक कैसे कर सकते हैं? या कभी आपको अपने आप को प्रोग्रामिंग करते हुए समस्याओं से जूझना पड़ता है? यदि हाँ, तो कोट्लिन में "standard error" पर लिखने के लिए सही समाधान हो सकता है। इससे आपको आपके कोड की समस्याओं का पता लगाने में मदद मिल सकती है।

## कैसे करें

```Kotlin
fun main(args: Array<String>) {
    println("Hello World!")
    System.err.println("This is an error message.")
}
```

उपरोक्त कोड का नतीजा निम्नलिखित हो सकता है:

```
Hello World!
This is an error message.
```

आप कोड में `System.err.println()` का उपयोग करके समस्याओं को लोगों तक पहुंचा सकते हैं। इससे आपको अपने कोड को ठीक करने में आसानी होगी।

## गहराई में जाएँ

"standard error" को आमतौर पर एसई के रूप में जाना जाता है और यह उपयोगकर्ता द्वारा दी गई त्रुटियों को दर्शाता है। इससे आपको अपने कोड में सुधार के लिए आसानी होगी। समझने के लिए, आप अपने कोड में `System.out.println()` और `System.err.println()` के अंतर को निम्नलिखित उदाहरण में देख सकते हैं:

```Kotlin
fun main(args: Array<String>) {
    println("This is normal output.")
    System.err.println("This is an error message.")
}
```

इस कोड का नतीजा निम्नलिखित होगा:

```
This is normal output.
This is an error message.
```

अब अपने कोड में सही समझौते करने के लिए `System.err.println()` का उपयोग करें।

## देखें भी

- [Writing to Standard Error in Java](https://www.baeldung.com/java-system-standard-error-output)
- [Understanding Standard Error in Programming](https://www.geeksforgeeks.org/understanding-standard-error-in-programming/)