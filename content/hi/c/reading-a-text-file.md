---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:08.603937-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)
पाठ फ़ाइल को पढ़ना मतलब फ़ाइल से वर्णों का अनुक्रम निकालना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह डेटा एक्सेस, कॉन्फ़िगरेशन सेटिंग्स या यूज़र इनपुट के लिए ज़रूरी होता है। 

## How to: (कैसे करें:)
```C
#include <stdio.h>

int main() {
    FILE *file_pointer;
    char filename[] = "example.txt";
    char ch;

    file_pointer = fopen(filename, "r"); // 'r' का मतलब read mode

    if (file_pointer == NULL) {
        printf("फ़ाइल खुल नहीं पाई।\n");
        return 1;
    }

    while ((ch = fgetc(file_pointer)) != EOF) { // EOF मतलब End Of File
        putchar(ch);
    }

    fclose(file_pointer); // फ़ाइल को बंद करना ज़रूरी है 
    return 0;
}
```
सैंपल आउटपुट:
```
यह example.txt फ़ाइल का सामग्री है।
```

## Deep Dive (गहराई में जानकारी):
पाठ फ़ाइलों को पढ़ना C भाषा में सबसे बुनियादी ऑपरेशनों में से एक है और इसका इस्तेमाल 1970 के दशक से हो रहा है। `fopen`, `fgetc`, और `fclose` जैसे फंक्शन स्टैंडर्ड लाइब्रेरी में आते हैं। विकल्प के रूप में `fgets`, `fread`, और मॉडर्न C लाइब्रेरीज़ भी हैं। इसकी कार्यप्रणाली ऑपरेटिंग सिस्टम की फ़ाइल-हैंडलिंग तकनिकों पर आधारित होती है। कुशलता और सुरक्षा के लिहाज से, सही error चेकिंग और फ़ाइल हैंडल्स का समुचित प्रबंधन महत्वपूर्ण है।

## See Also (यह भी देखें):
- C Standard Library Reference: https://en.cppreference.com/w/c/io
- GNU C Library Documentation: https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html
- C File I/O tutorial: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
