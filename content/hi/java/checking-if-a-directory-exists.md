---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:57:37.843258-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी की जाँच करने के मतलब है, पता लगाना कि कोई फोल्डर सिस्टम में मौजूद है या नहीं। प्रोग्रामर्स यह इसलिए करते हैं ताकि गलतियों से बच सकें, जैसे कि उस फोल्डर में फाइल सेव करना जो है ही नहीं!

## कैसे करें:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        Path path = Paths.get("यहाँ-आपका-डायरेक्टरी-पथ");

        if (Files.exists(path)) {
            System.out.println("डायरेक्टरी मौजूद है: " + path);
        } else {
            System.out.println("डायरेक्टरी मौजूद नहीं है: " + path);
        }
    }
}
```
कोड चलने पर सैंपल आउटपुट होगा:
```
डायरेक्टरी मौजूद है: यहाँ-आपका-डायरेक्टरी-पथ
```
या
```
डायरेक्टरी मौजूद नहीं है: यहाँ-आपका-डायरेक्टरी-पथ
```

## गहराई से जानकारी:

पहले, `java.io.File` का इस्तेमाल करके डायरेक्टरी के होने की जाँच की जाती थी। हालांकि, Java NIO (New I/O) आने के बाद से `java.nio.file.Files` और `java.nio.file.Paths` प्रचलन में आ गए हैं क्योंकि इनमें ज्यादा कार्यक्षमता और संवेदनशीलता होती है। `Files.exists()` का उपयोग करके आप यह जान सकते हैं कि फाइल या डायरेक्टरी मौजूद है या नहीं, और यह ध्यान में रखते हुए कि कभी-कभी फाइल सिस्टम की गड़बड़ियों के कारण `exists()` गलत परिणाम दे सकता है, इसके साथ `notExists()` या `isReadable()` जैसे दूसरे चेक्स को भी इस्तेमाल करना चाहिए।

## और भी देखें:

- [Java NIO Files](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Files.html)
- [Java NIO Paths](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Paths.html)
- [Oracle Java Documentation](https://docs.oracle.com/en/java/)

इस लेख में सिखाई गई तकनीकें आपको फाइल सिस्टम के साथ काम करने में काफी मददगार साबित होंगी। इन लिंक्स की मदद से आप इस विषय में और भी गहराई से जान सकते हैं।