---
title:                "अस्थायी फाइल बनाना"
aliases:
- /hi/java/creating-a-temporary-file/
date:                  2024-01-20T17:41:15.789247-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
जावा में अस्थाई फाइल बनाने का मतलब है ऐसी फाइलें जिन्हें हम अस्थायी डेटा संग्रहण के लिए इस्तेमाल करते हैं। प्रोग्रामर्स तब अस्थाई फाइलें बनाते हैं, जब उन्हें ऐसे डेटा को स्टोर करने की जरूरत होती है जो केवल चालू सेशन के लिए महत्वपूर्ण है।

## कैसे करें: (How to:)
```java
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {
    public static void main(String[] args) {
        try {
            // एक अस्थाई फाइल बनाइए
            File tempFile = File.createTempFile("my_temp_file", ".txt");

            // अस्थाई फाइल का पथ प्रिंट करिए
            System.out.println("अस्थाई फाइल का पथ: " + tempFile.getAbsolutePath());

            // काम खत्म होने पर अस्थाई फाइल को डिलीट कर दीजिए
            tempFile.deleteOnExit();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
सैंपल आउटपुट:
```
अस्थाई फाइल का पथ: C:\Users\YourUsername\AppData\Local\Temp\my_temp_file1234567890.txt
```

## गहराई से जानकारी: (Deep Dive)
जावा में अस्थाई फाइलों का कंसेप्ट सिस्टम के टेम्प फोल्डर का इस्तेमाल करके शुरू हुआ। इसका उपयोग होता है डाटा को इंटरमीडिएट प्रोसेसिंग के लिए स्टोर करने और फिर सुरक्षित रूप से डिस्कार्ड करने में। कुछ विकल्प हैं `java.nio.file` पैकेज की `Files.createTempFile` मेथड, जहां आपको और अधिक कंट्रोल मिलता है फाइल क्रिएशन पर। यह मेथड आपको डायरेक्टरी का चुनाव करने, फाइल नाम की प्रीफिक्स और सफिक्स सेट करने, और फाइल के एट्रीब्यूट को कॉन्फिगर करने की सुविधा देता है।

## सम्बंधित जानकारी: (See Also)
- [Class File Documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Files (Java NIO)](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
