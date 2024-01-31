---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट फाइल लिखना मतलब जानकारी को फाइल में सेव करना। प्रोग्रामर्स ये करते हैं डेटा बैकअप, लॉगिंग, और कॉन्फ़िगरेशन सेटिंग्स के लिए।

## How to: (कैसे करें:)

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {
        String path = "example.txt";
        String content = "हेलो, यह एक परिचय है!";
        
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(path))) {
            bw.write(content);
            System.out.println("फ़ाइल सफलतापूर्वक लिखी गई।");
        } catch (IOException e) {
            System.err.println("एक त्रुटि हुई: " + e.getMessage());
        }
    }
}
```
सैंपल आउटपुट:
```
फ़ाइल सफलतापूर्वक लिखी गई।
```

## Deep Dive (गहराई में जानकारी):

पहले, `java.io` पैकेज इस्तेमाल होता था फाइल ऑपरेशन्स के लिए। अब, `java.nio` पैकेज बेहतर विकल्प है क्योंकि यह ज्यादा फ्लेक्सिबल और इफ़िशिएंट है। डेटा एन्कोडिंग, बफ़र साइज़, और एरर हैंडलिंग जैसी चीज़ों पर विचार करना जरूरी है टेक्स्ट फाइल लिखते समय।

## See Also (और भी देखें):

- [Oracle Java Docs for java.io.BufferedWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html)
- [Oracle Java Docs for java.nio.file.Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
