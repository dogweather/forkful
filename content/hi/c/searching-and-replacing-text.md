---
title:                "टेक्स्ट की खोज और प्रतिस्थापन"
html_title:           "C: टेक्स्ट की खोज और प्रतिस्थापन"
simple_title:         "टेक्स्ट की खोज और प्रतिस्थापन"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड कर्मियों के लिए, टेक्स्ट खोज और प्रतिस्थापन क्या है और वे इसे क्यों करते हैं।

## कैसे करें:
```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[50] = "Hello World!";
  
  // टेक्स्ट का खोज और प्रतिस्थापन
  printf("Before replacement: %s\n", str);
  strcpy(str, "Goodbye World!");
  printf("After replacement: %s\n", str);

  return 0;
}
```
उपलब्ध आउटपुट:
```
Before replacement: Hello World!
After replacement: Goodbye World!
```

## गहराई समीक्षा:
1. इतिहास कंटेक्स्ट: टेक्स्ट खोज और प्रतिस्थापन का बदलाव सुविधाजनक ्तरीके को मिलाकर करते हुए कर्मियों को कोडिंग काम को निश्चित रूप से हो सकते हैं।
2. विकल्प: अन्य भाषाओं में, कुछ अन्य काम में एक अलग ढंग से टेक्स्ट की खोज और प्रतिस्थापन के लिए कोड उपलब्ध हो सकते हैं।
3. अंतराल: टेक्स्ट की खोज और प्रतिस्थापन को समझाने के लिए, आपको कुछ प्रकार के स्ट्रिंग फ़ंक्शन्स के साथ संबंधित जानकारी होनी चाहिए।

## इस तरह:
- [विकिपीडिया - टेक्स्ट खोज और प्रतिस्थापन](https://en.wikipedia.org/wiki/find_and_replace)
- [C लैंग्वेज लर्निंग ट्यूटोरियल](https://www.programiz.com/c-programming/c-string-functions)