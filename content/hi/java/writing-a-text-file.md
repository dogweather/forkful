---
title:                "Java: एक टेक्स्ट फ़ाइल लिखना"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
तुम्हें किसी टेक्स्ट फ़ाइल को लिखने में क्यों रुचि हो सकती है, इसके बारे में जानने के लिए हमारे इस ब्लॉग पोस्ट पढ़ें।

## कैसे करें
अगर आप किसी भी टेक्स्ट फ़ाइल को एक जावा प्रोग्राम से लिखना चाहते हैं, तो आपको फ़ाइल और फ़ाइल लाइब्रेरी की मदद से उस फ़ाइल को बनाना होगा। नीचे आपको एक कोड उदाहरण दिया गया है:

```Java
import java.io.*;

public class FileWritingExample {

  public static void main(String[] args) {
    try {
      FileWriter writer = new FileWriter("myFile.txt"); // नये फ़ाइल बनाएं

      // फ़ाइल में डेटा लिखें
      writer.write("यह हमारी पहली टेक्स्ट फ़ाइल है।");

      // स्ट्रीम बंद करें
      writer.close();

      System.out.println("फ़ाइल सफ़लतापूर्वक बनाई गई है!");
    } catch (IOException e) {
      System.out.println("कुछ त्रुटि है।");
      e.printStackTrace();
    }
  }

}
```

मध्यांतर प्रकट क्रम पर:

```
यह हमारी पहली टेक्स्ट फ़ाइल है।
```

आपको `myFile.txt` नाम की फ़ाइल बनाई गई होगी जो कि आपके प्रोग्राम के साथ स्थानीय रूप से सहेजी गई होगी।

## गहराई में जाएं
टेक्स्ट फ़ाइल लिखने के लिए आपको `java.io` पैकेज का उपयोग करना होगा। यह पैकेज आपको फ़ाइल बनाने, लिखने और उसमें डेटा लिखने और पढ़ने के लिए विभिन्न कक्षाओं को प्रदान करता है। आप `FileWriter` कक्षा का उपयोग करके टेक्स्ट फ़ाइल बना सकते हैं और `FileReader` कक्षा का उपयोग करके उसमें से डेटा पढ़ सकते हैं।

## देखें भी
टेक्स