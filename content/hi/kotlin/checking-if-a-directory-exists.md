---
title:                "डायरेक्टरी मौजूद है या नहीं, इसकी जांच करें"
html_title:           "Gleam: डायरेक्टरी मौजूद है या नहीं, इसकी जांच करें"
simple_title:         "डायरेक्टरी मौजूद है या नहीं, इसकी जांच करें"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डायरेक्टरी मौजूद है या नहीं, इसे जांचना मतलब होता है कि क्या किसी विशेष डायरेक्टरी की उपस्थिति हमारे सिस्टम में है या नहीं। प्रोग्रामर इसे इसलिए करते हैं ताकि उन्हें यह सुनिश्चित हो सके कि फ़ाइल ऑपरेशन को करने के लिए डायरेक्टरी या फ़ाइल मौजूद है या नहीं।

## कैसे करें:
आओ कोडिंग उदाहरण और सैंपल आउटपुट देखें। डायरेक्टरी का अस्तित्व जांचने के लिए, आप निम्न कोड स्निपेट का उपयोग कर सकते हैं:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/users/yourName/yourDirectory")
    val exists = Files.exists(path)

    if (exists) {
        println("डायरेक्टरी मौजूद है।")
    } else {
        println("डायरेक्टरी मौजूद नहीं है।")
    }
}
```
यह कोड पहले `/users/yourName/yourDirectory` पथ का अस्तित्व जांचेगा। अगर यह मौजूद है, तो "डायरेक्टरी मौजूद है।" प्रिंट होगा। अन्यथा, "डायरेक्टरी मौजूद नहीं है।" प्रिंट होगा।

## गहरा डाइव:
(1) हिस्टोरिकल कॉंटेक्स्ट: डायरेक्टरी का अस्तित्व जांचने की आवश्यकता हमेशा से ही रही है, जब से कंप्यूटर प्रोग्रामिंग की शुरुआत हुई। यह एक महत्वपूर्ण तत्व है खराब फ़ाइल पथ के कारण कोड के विफल होने से बचने के लिए।

(2) विकल्प: कुछ केसेस में, आप भी `File` ऑब्जेक्ट का उपयोग करके डायरेक्टरी के अस्तित्व की जांच कर सकते हैं। लेकिन `java.nio.file` पैकेज मौजूदा कोट्लिन वर्जन में अधिक चुस्त और उत्कृष्ट है।

(3) इम्प्लीमेंटेशन विवरण: इस काम के लिए `java.nio.file` पैकेज का `Files.exists()` मेथड कोड में इस्तेमाल किया गया है। यह जांचता है कि एक फ़ाइल या डायरेक्टरी का निर्दिष्ट पथ मौजूद है या नहीं।

## देखें भी:
कृपया निम्नलिखित लिंक्स देखें जिसमें विषय से संबंधित और गहन जानकारी है:
- [Official Java™ Tutorials - Checking a File or Directory](https://docs.oracle.com/javase/tutorial/essential/io/check.html)
- [StackOverflow - How to check if a directory exists in Kotlin?](https://stackoverflow.com/questions/49173273/how-to-check-if-a-directory-exists-in-kotlin)