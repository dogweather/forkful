---
title:                "C: टेक्स्ट को खोजना और बदलना"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## वजह
यदि आप टेक्स्ट की खोज और बदलाव का सामना करना चाहते हैं, तो आपको प्रोग्रामिंग में दृष्टि से इससे बहुत फायदा होगा।

## कैसे करें
```C
#include <stdio.h>

int main() {
    // टेक्स्ट स्ट्रिंग
    char str[] = "हिंदी टेक्स्ट";

    // खोज का विकल्प
    char search = 'ट';

    // बदलने का विकल्प
    char replace = 'प';

    // खोज और बदलाव का लूप
    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == search)
            str[i] = replace;
    }

    // परिणाम दिखाएं
    printf("बदलाव किया गया टेक्स्ट: %s", str);

    return 0;
}
```

### आउटपुट:
```
बदलाव किया गया टेक्स्ट: हिंदी पेक्स्
```

## गहराई में जाएं

टेक्स्ट को खोज और बदलने का काम बहुत आसान लग सकता है, लेकिन इसमें गहराई समझने से बहुत फायदा होता है। जब हम कोडिंग के रूप में इस कोलेज के स्तर पर देखते हैं, तो हम प्रोग्राम्स के समझे सुलभ बदलावों को कार्यान्वित कर सकते हैं। यह में न केवल आपको प्रोफेशनल लगाता है, बल्कि आपको ज्यादा सकारात्मक रूप से सोचना स्किल को देता है।

## देखें भी
- [स्ट्रिंग मैनिपुलेसन के बारे में सी प्रोग्रामिंग कोस्येन्स्स](https://www.freecodecamp.org/news/string-manipulation-in-c-programming-with-examples/)
- [हिंदी स्ट्रिंग मैनिपुलेसन के बारे में](https://www.udebug.com/HINDISTRINGS)