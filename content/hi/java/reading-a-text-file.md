---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:40.913763-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
टेक्स्ट फाइल पढ़ना मतलब फाइल से डाटा को अपने जावा प्रोग्राम में लाना होता है। प्रोग्रामर इसे ऐसे अनेकविध कारणों से करते हैं, जैसे कि यूजर के डाटा को प्रोसेस करना, कॉन्फ़िगरेशन पढ़ना, या फिर रिपोर्ट्स जनरेट करना।

## कैसे करें? (How to:)
```Java
import java.nio.file.*;
import java.io.IOException;

public class FileReaderExample {

    public static void main(String[] args) {
        Path path = Paths.get("example.txt");

        try {
            String content = Files.readString(path);
            System.out.println("फाइल से पढ़ा गया डाटा है:");
            System.out.println(content);
        } catch (IOException e) {
            System.err.println("फाइल पढ़ने में एरर आ गया है।");
            e.printStackTrace();
        }
    }
}
```
सेंपल आउटपुट:
```
फाइल से पढ़ा गया डाटा है:
Hello, this is a text from the example file.
```

## गहराई से जानकारी (Deep Dive)
जावा में फाइल सिस्टम से इंटरेक्शन हम कई तरह से कर सकते हैं। शुरुआती जावा वर्शन में `FileInputStream`, `BufferedReader` जैसे क्लासेज थे। जावा 1.7 (नाम से भी जाना जाता है NIO.2) में `Files` और `Paths` क्लासेज ऐड किए गए जो ज्यादा एफिसिएंट और आसान तरीके से फाइल्स को हैंडल करते हैं। `Files.readString` और `Files.readAllLines` जैसे मेथड फ़ाइल की सामग्री को एक बार में पढ़ लेते हैं। इस सुविधा से कोड कम और साफ बनता है। विकल्पों में `Scanner`, `FileReader`, `BufferedInputStream` आदि शामिल हैं, जो अलग-अलग परिस्थितियों में उपयोगी हो सकते हैं जैसे कि बड़ी फाइल्स को पढ़ने के लिए या खास पैटर्न पर डाटा प्रोसेस करने के लिए।

## और भी देखें (See Also)
- जावा डॉक्युमेंटेशन पर `Files` क्लास: [Java SE Documentation](https://docs.oracle.com/en/java/javase/)
- फाइल I/O ट्यूटोरियल जावा डॉक्स: [Oracle File I/O Tutorial](https://docs.oracle.com/javase/tutorial/essential/io/)
- जावा I/O गाइड: [Baeldung Java I/O Guide](https://www.baeldung.com/java-io)