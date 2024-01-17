---
title:                "एक पाठ फ़ाइल को पढ़ना"
html_title:           "Java: एक पाठ फ़ाइल को पढ़ना"
simple_title:         "एक पाठ फ़ाइल को पढ़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
पाठ फ़ाइल पढ़ना क्या होता है और क्यों कोडर्स इसे करते हैं? पाठ फ़ाइल एक साधारण टेक्स्ट फाइल होती है जिसमें पाठ के अलावा कोई अन्य जानकारी नहीं होती है। कोडर्स इन फाइलों को पढ़कर और उसमें दिए गए डाटा को प्रोग्राम में उपयोग करके अपने कोड को और सुधार सकते हैं।

## कैसे?
```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TextFileReader {

  public static void main(String[] args) {
    // फ़ाइल नाम और उसका पथ दर्ज करें
    File file = new File("/Users/MyDocuments/example.txt");

    try {
      // FileReader का इस्तेमाल करके फाइल खोलें
      Scanner sc = new Scanner(file);

      // उपलब्ध टेक्स्ट फ़ाइल पढ़ें
      while (sc.hasNextLine()) {
        String data = sc.nextLine(); // प्रत्येक लाइन को स्ट्रिंग में रखने के लिए
        System.out.println(data); // लाइन प्रिंट करें
      }
      // स्कैनर बंद करें
      sc.close();
    } catch (FileNotFoundException e) {
      System.out.println("फ़ाइल नहीं मिली!");
      e.printStackTrace();
    }
  }
}
```

उपरोक्त कोड रन करने पर निम्नलिखित प्रिंटआउट होगा:
```
Hello World!
This is a sample text file.
```

## गहराई में
इतिहास के संदर्भ में, पाठ फ़ाइल पढ़ने की तकनीक हाल के वर्षों में काफी बदल चुकी है। पहले, फ़ाइलें कुछ व्यक्तिगत डेटा को स्टोर करने के लिए उपयोग की जाती थीं। परंतु आजकल कई बड़े फ़ाइल प्रोग्राम्स डेटा को प्रोसेस करने के लिए तीसरे पक्ष को स्टोर करने के लिए पाठ फ़ाइलों का उपयोग करते हैं। पाठ फ़ाइलें बहुत सरल होती हैं और इसलिए कोडर्स को अपने प्रोग्राम को पढ़ना और प्रोसेस करना आसान होता है।

यदि आपको एक धीमी उपकरणा चाहिए हो या अधिक कंप्लेक्स तकनीकों का उपयोग करना हो, तो आप अलग-अलग फ़ाइल पढ़ने की तकनीकों को अपने कोड में इम्प्लीमेंट कर सकते हैं। अपनी जरूरतों के अनुसार आप टेक्स्ट फाइल को स्ट्रिंग के रूप में पढ़ सकते हैं या अलग-अलग डेटा टाइप्स की समीकरण कर सकते हैं।

## देखें भी
- [जावा रीडिंग ए टेक्स्ट फाइल (हिंदी)](https://www.w3schools.com/java/java_files_read.asp)
- [java.io.FileReader क्लास (हिंदी)](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)