---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:19.513901-07:00
description: "\u0915\u0948\u0938\u0947: Java \u0915\u093E \u0928\u094D\u092F\u0942\
  \ I/O (NIO) \u092A\u0948\u0915\u0947\u091C (`java.nio.file`) \u092B\u093E\u0907\u0932\
  \u094B\u0902 \u0938\u0947 \u0928\u093F\u092A\u091F\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0905\u0927\u093F\u0915 \u092C\u0939\u0941\u092E\u0941\u0916\u0940\
  \ \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901\
  \ `Files.write()` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u090F\u0915\u2026"
lastmod: '2024-03-13T22:44:52.145640-06:00'
model: gpt-4-0125-preview
summary: "Java \u0915\u093E \u0928\u094D\u092F\u0942 I/O (NIO) \u092A\u0948\u0915\u0947\
  \u091C (`java.nio.file`) \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u0928\
  \u093F\u092A\u091F\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0905\u0927\u093F\
  \u0915 \u092C\u0939\u0941\u092E\u0941\u0916\u0940 \u0926\u0943\u0937\u094D\u091F\
  \u093F\u0915\u094B\u0923 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 `Files.write()` \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u090F\u0915 \u092B\u093E\
  \u0907\u0932 \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u093E \u090F\
  \u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

## कैसे:


### `java.nio.file` का उपयोग (मानक पुस्तकालय)
Java का न्यू I/O (NIO) पैकेज (`java.nio.file`) फाइलों से निपटने के लिए अधिक बहुमुखी दृष्टिकोण प्रदान करता है। यहाँ `Files.write()` का उपयोग करके एक फाइल में लिखने का एक सरल तरीका है:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("फाइल सफलतापूर्वक लिखी गई!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

आउटपुट:

```
फाइल सफलतापूर्वक लिखी गई!
```

### `java.io` का उपयोग (मानक पुस्तकालय)
एक अधिक पारंपरिक दृष्टिकोण के लिए, `java.io.FileWriter` सरलता से टेक्स्ट फाइलें लिखने के लिए एक अच्छा विकल्प है:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hello, World!\n");
            writer.append("This is another line.");
            System.out.println("फाइल सफलतापूर्वक लिखी गई!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

आउटपुट:

```
फाइल सफलतापूर्वक लिखी गई!
```

### Apache Commons IO का उपयोग
Apache Commons IO लाइब्रेरी कई ऑपरेशनों, जिसमे फाइल लेखन शामिल है, को सरल बनाती है। यहाँ `FileUtils.writeStringToFile()` का उपयोग करके एक फाइल में लिखने का तरीका है:

पहले, अपनी परियोजना में निर्भरता शामिल करें। यदि Maven का उपयोग कर रहे हैं, तो शामिल करें:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- नवीनतम संस्करण के लिए जांचें -->
</dependency>
```

फिर, एक फाइल में टेक्स्ट लिखने के लिए निम्नलिखित कोड का उपयोग करें:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "This is text written using Commons IO.", "UTF-8");
            System.out.println("फाइल सफलतापूर्वक लिखी गई!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

आउटपुट:

```
फाइल सफलतापूर्वक लिखी गई!
```
