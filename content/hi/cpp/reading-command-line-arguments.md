---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:15.716972-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट्स पढ़ना यानी उन मानों को पकड़ना जो एक प्रोग्राम को उसके चालू होने के समय दी जाती हैं। प्रोग्रामर इसलिए ऐसा करते हैं क्योंकि इससे उन्हें अलग-अलग परिस्थितियों में उनके प्रोग्राम को कस्टमाइज करने की सुविधा मिलती है।

## How to: (कैसे करें:)
```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "प्रोग्राम को मिले आर्ग्यूमेंट्स की संख्या: " << argc << std::endl;
    for (int i = 0; i < argc; ++i) {
        std::cout << "आर्ग्यूमेंट " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```
सैंपल आउटपुट अगर प्रोग्राम को `./myProgram Hello World` के साथ चलाया जाए:
```
प्रोग्राम को मिले आर्ग्यूमेंट्स की संख्या: 3
आर्ग्यूमेंट 0: ./myProgram
आर्ग्यूमेंट 1: Hello
आर्ग्यूमेंट 2: World
```

## Deep Dive (गहराई से जानकारी)
कमांड लाइन आर्ग्यूमेंट्स का उपयोग सबसे पहले कंप्यूटर प्रोग्रामिंग के शुरुआती दिनों से चला आ रहा है। 

1. **इतिहास**: UNIX जैसे ऑपरेटिंग सिस्टम्स ने इस तरीके को लोकप्रिय बनाया क्योंकि वो कमांड लाइन पर निर्भर थे। 
2. **विकल्प**: आपको कमांड लाइन लाइब्रेरीज जैसे `Boost.Program_options` और `getopt` मिलेंगे जो कि आर्ग्यूमेंट पार्सिंग में उपयोगी हैं और अधिक फंक्शनैलिटी देते हैं। 
3. **कार्यान्वित करने की जानकारी**: `argc` (आर्ग्यूमेंट काउंट) बताता है कि कितने आर्ग्यूमेंट्स पास किए गए हैं और `argv` (आर्ग्यूमेंट वेक्टर) उन आर्ग्यूमेंट्स के वैल्यूज को स्टोर करता है।

## See Also (और भी जानकारी)
- C++ ISO Standard: https://isocpp.org/
- GNU `getopt`: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- Boost Program_options: https://www.boost.org/doc/libs/release/libs/program_options/

प्रोग्रामिंग में लगे रहें, अच्छा कोड लिखते रहें, और कुछ भी नया सीखते रहें!
