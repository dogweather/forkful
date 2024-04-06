---
date: 2024-01-20 17:54:40.913763-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u091C\u093E\
  \u0935\u093E \u092E\u0947\u0902 \u092B\u093E\u0907\u0932 \u0938\u093F\u0938\u094D\
  \u091F\u092E \u0938\u0947 \u0907\u0902\u091F\u0930\u0947\u0915\u094D\u0936\u0928\
  \ \u0939\u092E \u0915\u0908 \u0924\u0930\u0939 \u0938\u0947 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u0936\u0941\u0930\u0941\u0906\u0924\u0940\
  \ \u091C\u093E\u0935\u093E \u0935\u0930\u094D\u0936\u0928 \u092E\u0947\u0902 `FileInputStream`,\
  \ `BufferedReader` \u091C\u0948\u0938\u0947 \u0915\u094D\u0932\u093E\u0938\u0947\
  \u091C\u2026"
lastmod: '2024-04-05T22:51:06.833297-06:00'
model: gpt-4-1106-preview
summary: ") \u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u092B\u093E\u0907\u0932 \u0938\
  \u093F\u0938\u094D\u091F\u092E \u0938\u0947 \u0907\u0902\u091F\u0930\u0947\u0915\
  \u094D\u0936\u0928 \u0939\u092E \u0915\u0908 \u0924\u0930\u0939 \u0938\u0947 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0936\u0941\u0930\u0941\
  \u0906\u0924\u0940 \u091C\u093E\u0935\u093E \u0935\u0930\u094D\u0936\u0928 \u092E\
  \u0947\u0902 `FileInputStream`, `BufferedReader` \u091C\u0948\u0938\u0947 \u0915\
  \u094D\u0932\u093E\u0938\u0947\u091C \u0925\u0947\u0964 \u091C\u093E\u0935\u093E\
  \ 1.7 (\u0928\u093E\u092E \u0938\u0947 \u092D\u0940 \u091C\u093E\u0928\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948 NIO.2) \u092E\u0947\u0902 `Files` \u0914\u0930 `Paths`\
  \ \u0915\u094D\u0932\u093E\u0938\u0947\u091C \u0910\u0921 \u0915\u093F\u090F \u0917\
  \u090F \u091C\u094B \u091C\u094D\u092F\u093E\u0926\u093E \u090F\u092B\u093F\u0938\
  \u093F\u090F\u0902\u091F \u0914\u0930 \u0906\u0938\u093E\u0928 \u0924\u0930\u0940\
  \u0915\u0947 \u0938\u0947 \u092B\u093E\u0907\u0932\u094D\u0938 \u0915\u094B \u0939\
  \u0948\u0902\u0921\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964 `Files.readString`\
  \ \u0914\u0930 `Files.readAllLines` \u091C\u0948\u0938\u0947 \u092E\u0947\u0925\u0921\
  \ \u095E\u093E\u0907\u0932 \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940\
  \ \u0915\u094B \u090F\u0915 \u092C\u093E\u0930 \u092E\u0947\u0902 \u092A\u0922\u093C\
  \ \u0932\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u0907\u0938 \u0938\u0941\u0935\
  \u093F\u0927\u093E \u0938\u0947 \u0915\u094B\u0921 \u0915\u092E \u0914\u0930 \u0938\
  \u093E\u092B \u092C\u0928\u0924\u093E \u0939\u0948\u0964 \u0935\u093F\u0915\u0932\
  \u094D\u092A\u094B\u0902 \u092E\u0947\u0902 `Scanner`, `FileReader`, `BufferedInputStream`\
  \ \u0906\u0926\u093F \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0902, \u091C\u094B\
  \ \u0905\u0932\u0917-\u0905\u0932\u0917 \u092A\u0930\u093F\u0938\u094D\u0925\u093F\
  \u0924\u093F\u092F\u094B\u0902 \u092E\u0947\u0902 \u0909\u092A\u092F\u094B\u0917\
  \u0940 \u0939\u094B \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\u0948\u0938\
  \u0947 \u0915\u093F \u092C\u0921\u093C\u0940 \u092B\u093E\u0907\u0932\u094D\u0938\
  \ \u0915\u094B \u092A\u095D\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092F\u093E\
  \ \u0916\u093E\u0938 \u092A\u0948\u091F\u0930\u094D\u0928 \u092A\u0930 \u0921\u093E\
  \u091F\u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F\u0964."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
