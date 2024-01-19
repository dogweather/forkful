---
title:                "डायरेक्टरी मौजूद है या नहीं, इसकी जाँच करना"
html_title:           "Java: डायरेक्टरी मौजूद है या नहीं, इसकी जाँच करना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं, इसकी जाँच करना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी मौजूद है या नहीं, इसकी जांच करने का कार्य एक आम समस्या है जिसे प्रोग्रामर्स हल करते हैं। इसे जांचना एक क्लीन और मुकाबले के योग्य कोड का एक महत्वपूर्ण तत्व है क्योंकि यह बाद में जटिलताओं और त्रुटियों से बचता है। 

## कैसे करें:

एक डायरेक्टरी मौजूद है या नहीं, इसे जांचने के लिए हम जावा में `Files` क्लास का उपयोग कर सकते हैं।

```Java
import java.nio.file.*;

public class CheckDirectory {
    public static void main(String[] args) {
        Path path = Paths.get("/path/to/directory");

        if (Files.exists(path)) {
            System.out.println("The directory exists.");
        } else {
            System.out.println("The directory doesn't exist.");
        }
    }
}
```
इस कोड का आउटपुट निम्नवत होगा:

```
The directory exists.
```

अगर पथ मौजूद नहीं होता, तो यह कहेगा: `"The directory doesn't exist."`

## गहराई में:

Java की पहली संस्करणों में, हमें इसे `java.io.File` क्लास के `exists` मेथड के माध्यम से करना पड़ता था। लेकिन, Java NIO पैकेज के आगमन के साथ, हम अब `java.nio.file.Files` क्लास का उपयोग कर सकते हैं, जो एक अधिक शक्तिशाली और लचीली उपकरण है। 

इसके विकल्प के रूप में `File.exists()` का उपयोग भी किया जा सकता है, जैसा कि पहले उल्लिखित किया गया था, लेकिन `Files.exists()` का उपयोग करनादेखभाल में वीचार करने वाले प्रमुख तत्व है।

## देखें भी:

- [Oracle Docs: Java NIO Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Java Tutorials: File I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [StackOverflow: Checking if a file or directory exists](https://stackoverflow.com/questions/775395/whats-the-best-way-to-check-if-a-file-exists-in-scratch-java)