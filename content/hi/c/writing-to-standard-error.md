---
title:                "C: स्टैंडर्ड एरर पर लिखना"
simple_title:         "स्टैंडर्ड एरर पर लिखना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों
क्यूँ हम किसी स्टैंडर्ड एरर मैसेज को लिखने में शामिल होना चाहेंगे?

## कैसे करें
```
C #include <stdio.h> 

int main() 
{ 
    fprintf(stderr, "यह एक स्टैंडर्ड एरर का संदेश है!"); // एरर मैसेज को लिखने के लिए इस्तेमाल किया जाता है। 
    return 0; 
}
``` 
आउटपुट: यह एक स्टैंडर्ड एरर का संदेश है!

## गहरा जानकारी 
स्टैंडर्ड एरर मैसेज लिखने के लिए `stderr` का उपयोग किया जाता है जो कि कंप्यूटर प्रोग्राम के पिक्सल स्क्रीन में एक तरह से चलता है। यह मुख्य रूप से एक समस्या को दिखाने के लिए प्रयुक्त होता है लेकिन आमतौर पर इसे कोड की रोगदंश प्रस्तुत करने के लिए भी उपयोग किया जाता है।

## देखें भी 
- [C भाषा परिचय](https://www.programiz.com/c-programming)
- [एरर मैसेज के बारे में और अधिक जानकारी](https://www.geeksforgeeks.org/error-handling-c/)
- [Standard Error को प्रिंट करना](https://www.freecodecamp.org/news/standard-error-print-in-c/)