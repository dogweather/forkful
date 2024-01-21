---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:18.490965-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग को लोअर केस में बदलना मतलब हर अक्षर को छोटा (lowercase) अक्षर में परिवर्तित करना है। प्रोग्रामर्स इसका इस्तेमाल डाटा संग्रहण, तुलना और प्रोसेसिंग में करते हैं ताकि इसमें स्थिरता आए और अक्षरों की परवाह किए बिना स्ट्रिंग्स का मिलान हो सके।

## How to: (कैसे करें:)

नीचे C कोड का उदाहरण दिया गया है, जो स्ट्रिंग को लोअर केस में बदलता है:

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char myString[] = "Hello, World!";
    toLowerCase(myString);
    printf("Lowercase String: %s\n", myString);
    return 0;
}
```

उदाहरण आउटपुट:

```
Lowercase String: hello, world!
```

## Deep Dive (गहराई से जानकारी)

स्ट्रिंग्स को लोअर केस में बदलने का अभ्यास काफी पुराना है, प्रोग्रामिंग की शुरुआती दौर से ही यह उपयोग में आ रहा है। `tolower` फंक्शन ANSI C स्टैण्डर्ड (C89) का हिस्सा है और `ctype.h` हेडर फाइल में घोषित होता है। विकल्प के तौर पर, कुछ प्रोग्रामर्स ASCII मानों के उपयोग से स्वयं इसे लागू करते हैं, लेकिन इससे पोर्टेबिलिटी में समस्या हो सकती है। `tolower` का उपयोग इसके लिए अधिक सुरक्षित और पोर्टेबल तरीका है।

## See Also (और देखें)

- C Standard Library Reference: [`tolower`](https://en.cppreference.com/w/c/string/byte/tolower)
- C String Handling (गाइड): [String Handling in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)