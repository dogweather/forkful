---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

(What & Why?)

एक स्ट्रिंग को लोअर केस में बदलना मतलब होता है कि हम सभी अक्षरों को छोटे (लोअर केस) में बदल देते हैं। यह उन समयों में उपयोगी होता है जब हम उपयोगकर्ता की इनपुट के सापेक्ष डाटा का मेल खाते हैं और यह सुनिश्चित करना चाहते हैं कि बड़े /छोटे अक्षर आपके डाटा के मेल खाने में खलल नहीं डाल रहे हैं। 

## कैसे:

(How to:)

आप `tolower()` फ़ंक्शन का उपयोग करके स्ट्रिंग को लोअर केस में कन्वर्ट कर सकते हैं।

```C
#include <ctype.h>
#include <stdio.h>

int main() {
    char s[] = "HELLO, WORLD!";
    for(int i = 0; s[i]; i++){
        s[i] = tolower(s[i]);
    }
    printf("%s\n", s);
    return 0;
}
```

उपरोक्त कोड का आउटपुट होगा:

```
hello, world!
```

## गहराई में:

(Deep Dive)

1. *ऐतिहासिक संदर्भ (Historical Context):* `tolower` और `toupper` फ़ंक्शन्स C के `ctype.h` लाइब्रेरी में उपलब्ध हैं, जो C89 से मौजूद हैं।

2. *विकल्प (Alternatives):* आप ASCII का उपयोग करके मैन्युअली यह कार्रवाई कर सकते हैं। लेकिन, `tolower()` फ़ंक्शन का उपयोग करना अधिक पसंदीदा और कुशल तरीका है।

3. *अंदरूनी विवरण (Implementation Details):* `tolower()`  फ़ंक्शन ASCII पर मूल रूप से निर्भर करता है जहां लोअर केस और अपर केस अक्षरों के बीच 32 का अंतर होता है।

## भी देखें:

(See Also:)

1. [C++ String to Lower Case](https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/)
2. [C String Functions](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
3. [C Programming](https://www.w3schools.com/C/)