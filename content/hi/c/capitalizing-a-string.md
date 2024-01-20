---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

स्ट्रिंग को कैपिटलाइज़ करना इसके अक्षरों को बड़े अक्षरों में बदलने का काम होता है। प्रोग्रामर इसे उद्घोष, उपयोगकर्ता की सहायता और स्थिरता के लिए करते हैं। 

## कैसे करें:

```C
#include <ctype.h>
#include <stdio.h>

void capitalize(char s[]) {
    for(int i = 0; s[i] != '\0'; i++) {
      s[i] = toupper(s[i]);
    }
}

int main() {
    char str[] = "programming is fun!";
    capitalize(str);
    printf("%s\n", str);  // Prints: PROGRAMMING IS FUN!

    return 0;
}
```
## गहराई में:

(1) ऐतिहासिक प्रशंग: इस तरीके का उपयोग के लम्बे समय से किया जा रहा है, यह ASCII मानक में बड़े एवं छोटे अक्षरों के बीच में अंतर को बढ़ाने के लिए किया जाता है।

(2) विकल्प: आप यदि स्क्रिप्ट भाषाओं (जैसे पायथन या जावास्क्रिप्ट) उपयोग कर रहे हैं तो आप 'toUpperCase()' जैसे फ़ंक्शन का उपयोग कर सकते हैं।

(3) कैसे काम करता है: toupper() फ़ंक्शन का उपयोग ASCII मानक के अनुसार स्ट्रिंग के प्रत्येक कैरेक्टर को बड़े अक्षर में बदलने के लिए किया जाता है। 

## संबंधित लिंक्स:

1. ASCII परिचय: http://www.asciitable.com/
2. ctype.h लाइब्रेरी डिटेल्स: https://www.cplusplus.com/reference/cctype/
3. जावास्क्रिप्ट toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase