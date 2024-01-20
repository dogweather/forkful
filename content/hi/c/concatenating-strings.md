---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? - What & Why?

स्ट्रिंग्स को मिलाना (concatenating strings) का मतलब दो या दो से अधिक स्ट्रिंग्स को जोड़ना है। प्रोग्रामर्स इसे डाटा को संगठित रखने और उपलब्ध कराने के लिए करते हैं।

## कैसे - How to:

स्ट्रिंग्स को C में मिलाने के लिए, हम `strcat` फ़ंक्शन का उपयोग करेंगे। नीचे एक साधारण कोड दिया गया है:  

```C
#include <stdio.h>
#include <string.h>

void main() {
    char str1[100] = "Hello, ";
    char str2[] = "world!";
    strcat(str1, str2);
    printf("%s\n", str1);
}
```
जब आप इसे चलाते हैं, आपको निम्नलिखित आउटपुट मिलेगा:  
```C
Hello, world!
```

## गहराई में: Deep Dive

"concatenation" शब्द "catena" से आया है जिसका अर्थ होता है chain। कम्प्यूटर साइंस में इसका इस्तेमाल स्ट्रिंग्स को जोड़ने के लिए किया जाता है। `strcat` फ़ंक्शन ने हमें यह सुविधा प्रदान की है, लेकिन इसका उपयोग ध्यान से करना चाहिए क्योंकि यह buffer overflow की समस्या उत्पन्न कर सकता है। इसके बदले, आप `strncat` का उपयोग कर सकते हैं, जो सुरक्षित विकल्प है क्योंकि यह आपको जोड़ने के लिए चरित्रों की संख्या को सीमित करता है।

## देखिए भी - See Also:

- [C string handling](https://en.wikipedia.org/wiki/C_string_handling)
- [C Programming/String manipulation](https://en.wikibooks.org/wiki/C_Programming/String_manipulation)