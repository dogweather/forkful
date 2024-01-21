---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:04:07.754345-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

नया प्रोजेक्ट शुरू करना C में एक खाली canvas है; खाली फाइल में आपके विचारों को कोड में बदलना। प्रोग्रामर नई समस्याओं का समाधान करने, नई तकनीक सीखने, या बस मनोरंजन के लिए यह करते हैं।

## How to: (कैसे करें:)

नया C प्रोजेक्ट बनाना बेसिक है। `main.c` नामक फाइल बनाएँ और निम्नलिखित कोड लिखें:

```c
#include <stdio.h>

int main() {
    printf("नमस्ते, नया प्रोजेक्ट!\n");
    return 0;
}
```

कंपाइल करने के लिए:

```bash
gcc main.c -o myproject
```

अब रन करें:

```bash
./myproject
```

आउटपुट होगा:

```
नमस्ते, नया प्रोजेक्ट!
```

## Deep Dive (गहराई से जानकारी)

C भाषा का इतिहास 1970 के दशक में शुरू हुआ था। Dennis Ritchie ने इसे Bell Labs में विकसित किया। यह सीधी और शक्तिशाली भाषा है, जिसके कारण यह सिस्टम प्रोग्रामिंग और एम्बेडेड सिस्टम्स में प्रचलित है।

वैकल्पिक रूप में, Python, Java, या Rust जैसी भाषाओं का उपयोग किया जा सकता है, लेकिन C की स्पीड और कंट्रोल अनूठी है।

नया प्रोजेक्ट C में शुरू करते समय, `gcc` या `clang` जैसे कंपाइलर्स, स्टैंडर्ड लाइब्रेरीज, और डीबगिंग टूल्स जैसे `gdb` का पता होना महत्वपूर्ण है।

## See Also (और भी देखें)

- C प्रोग्रामिंग ट्यूटोरियल: [Learn-C.org](https://www.learn-c.org/)
- C स्टैंडर्ड लाइब्रेरी रेफरेंस: [cppreference.com](https://en.cppreference.com/w/c/header)
- gcc कंपाइलर: [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
- clang कंपाइलर: [Clang Compiler](https://clang.llvm.org/)