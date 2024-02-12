---
title:                "एक टेक्स्ट फ़ाइल लिखना"
date:                  2024-02-03T19:29:19.513901-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Java में एक टेक्स्ट फाइल लिखना, फाइल सिस्टम पर फाइलों में सामग्री बनाने और लिखने के लिए भाषा की क्षमताओं का उपयोग करने के बारे में है। प्रोग्रामर इसे विभिन्न कारणों से करते हैं, जैसे कि लॉगिंग, डेटा निर्यात करना, या बाद की पुनर्प्राप्ति के लिए एप्लिकेशन स्थिति को सहेजना।

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
