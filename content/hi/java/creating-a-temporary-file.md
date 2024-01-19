---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

अस्थायी फ़ाइल बनाना, यह एक ऐसी फ़ाइल होती है जिसे सिस्टम के इस्तेमाल के बाद आप स्वतः हटा सकते हैं। प्रोग्रामर इसे डाटा को अल्पकालिन रूप से स्टोर करने के लिए बनाते हैं, जैसे कि: लॉग जानकारी, कैशेड डाटा, अथवा किसी बड़ी कार्यवाही की मध्यवर्ती परिणाम।

## कैसे (How to)

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile(null, ".mytemp");
            System.out.println("Temporary file path is: "+ tempFile.toAbsolutePath());
        
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

ऊपर दी गई Java प्रोग्राम एक अस्थायी फ़ाइल बनाती है। संभावित आउटपुट होता है:

```shell
Temporary file path is: /tmp/1234567890.mytemp
```

## गहराी की जानकारी (Deep Dive)

Java में अस्थायी फ़ाइलें बनाने का तरीका JDK 1.2 से उपलब्ध है और यह आवश्यक सुरक्षा के मानकों का पालन करती है। वैकल्पिक रूप से, कस्टम निर्देशिका और प्रारम्भिक नाम उपयोग करके अस्थायी फ़ाइलें बनाई जा सकती हैं। Java निम्न स्तर में अस्थायी फ़ाइलें उत्पन्न करने में सक्षम होता है, जो प्रणाली-निर्भर होती है और इससे फ़ाइलों की योग्यता और सुरक्षा बढ़ती है।

## और भी देखें (See Also)

[Oracle: The Java Tutorials - File I/O (Featuring NIO.2)](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)

[Geeks for Geeks: How to create a temporary file in Java](https://www.geeksforgeeks.org/create-temporary-file-java/)

[Jenkov.com: Java Temp File (java.io)](http://tutorials.jenkov.com/java-io/temporary-files.html)