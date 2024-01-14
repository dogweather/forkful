---
title:    "Java: टेक्स्ट फ़ाइल लिखना।"
keywords: ["Java"]
---

{{< edit_this_page >}}

# क्यों
टेक्स्ट फाइल लिखने को कुछ कारणों से लोगों को संलग्न करना चाहिए।

## कैसे करें
कॉडिंग उदाहरण और " ```जावा ... ```" कोड ब्लॉक के भीतर नमूना आउटपुट।

```java
// टेक्स्ट फाइल बनाने का उदाहरण

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class TextFileExample {
   public static void main(String[] args) {
      try {
         File myFile = new File("myTextFile.txt");
         if (myFile.createNewFile()) {
            System.out.println("तकनीकी सफलता।");
         } else {
            System.out.println("फाइल पहले से मौजूद है।");
         }
         FileWriter myWriter = new FileWriter("myTextFile.txt");
         myWriter.write("यह एक पाठ फाइल है।");
         myWriter.close();
         System.out.println("फाइल सफलतापूर्वक लिखी गई।");
      } catch (IOException e) {
         System.out.println("तकनीकी त्रुटि हुई।");
         e.printStackTrace();
      }
   }
}
```

आउटपुट:

```
टेक्स्ट फ़ाइल सफलतापूर्वक लिखी गई।
```

## गहराई में जाएं
टेक्स्ट फाइल बनाने के लिए `java.io.File` का उपयोग किया जाता है। इसके अलावा, फाइल लिखने के लिए `java.io.FileWriter` का उपयोग किया जाता है। एक नई फाइल बनाने के लिए `createNewFile()` विधि का उपयोग किया जाता है। यह `IOException` को उठाता है यदि फाइल पहले से मौजूद है। फाइल खोलने, लिखने और सफलतापूर्वक बंद करने के लिए `FileWriter.write()` और `FileWriter.close()` विधियों का उपयोग किया जाता है। आप अपनी फाइल को अन्य तरीकों से भी संपादित कर सकते हैं, जैसे कि `java.io.BufferedWriter` का उपयोग करके लंबी फाइलों को लिखने के लिए।

# देखें भी
- [Java और टेक्स्ट फाइलों के आसपास](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Java में फ़ाइल को लिखना, पढ़ना,