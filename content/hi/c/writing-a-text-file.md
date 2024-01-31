---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट फाइल लिखना यानी कंप्यूटर पर डेटा को सादे टेक्स्ट के रूप में सेव करना है। यह प्रोग्रामर्स के लिए जरूरी है क्योंकि इससे डेटा को आसानी से सेव, साझा और पुनः प्रयोग किया जा सकता है।

## How to: (कैसे करें?)
```C
#include <stdio.h>

int main() {
    FILE *filePointer;
    filePointer = fopen("example.txt", "w");

    if (filePointer == NULL) {
        printf("File can't be opened.\n");
        return 1;
    }

    fprintf(filePointer, "Hello, World!\n");
    fprintf(filePointer, "C programming language is fun.\n");

    fclose(filePointer);
    
    printf("File written successfully.\n");
    return 0;
}
```
सैंपल आउटपुट:
```
File written successfully.
```

## Deep Dive (गहराई से जानकारी)
पिछले समय से टेक्स्ट फाइलें प्रोग्रामिंग में डेटा सेव करने का एक मानक तरीका बन गई हैं। इन्हे लिखने के लिए `fprintf`, `fputs` जैसे फंक्शंस उपयोग में लाए जाते हैं। विकल्पों में बाइनरी फाइलें, XML, या JSON लिखना शामिल हैं, पर टेक्स्ट फाइलें आमतौर पर आसान और पठनीय होती हैं। इम्प्लिमेंटेशन डीटेल्स शामिल हैं फाइल पॉइंटर इनिशियलाइजेशन, एरर चेकिंग, और फाइल क्लोजिंग सही तरीके से।

## See Also (और भी जानें)
- C स्टैंडर्ड लाइब्रेरी के डॉक्युमेंटेशन: http://www.cplusplus.com/reference/cstdio/
- फाइल I/O ट्यूटोरियल्स: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- अडवांस्ड टॉपिक्स जैसे फाइल लॉकिंग: https://www.gnu.org/software/libc/manual/html_node/File-Locks.html
