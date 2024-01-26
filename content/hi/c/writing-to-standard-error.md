---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
C प्रोग्राम में, स्टैंडर्ड एरर (stderr) पर लिखने से हमें एरर मैसेजेस को आउटपुट स्ट्रीम से अलग रखने की सुविधा मिलती है। प्रोग्रामर्स ऐसा इसलिए करते हैं ताकि डेटा और एरर मेसेजेस में भेदभाव रखा जा सके और लॉगिंग और डिबगिंग में आसानी हो।

## How to: (कैसे करें:)
आइये देखें C लैंग्वेज के कोड के जरिए कैसे stderr पर लिखा जाता है:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "यह एक एरर मैसेज है।\n");
    return 0;
}
```

यदि आप इसे रन करते हैं, तो आपको निम्नलिखित आउटपुट मिलेगा:

```
यह एक एरर मैसेज है।
```

## Deep Dive (गहराई में जानकारी):
स्टैंडर्ड एरर, वास्तव में एक फाइल स्ट्रीम है जो C लैंग्वेज में `stderr` नामक ग्लोबल वेरिएबल के माध्यम से पहचानी जाती है। इसका इस्तेमाल 1970 के दशक से हो रहा है। वैकल्पिक रूप से, आप `write` सिस्टम कॉल का भी प्रयोग कर सकते हैं। `stderr` अक्सर बफर्ड नहीं होता है, जिसका अर्थ है कि मैसेजेस तुरंत कंसोल पर प्रकाशित होते हैं।

## See Also (ये भी देखें):
- GNU C Library का Documentation: https://www.gnu.org/software/libc/documentation.html
- C Standard Library के Function: https://en.cppreference.com/w/c/io
- POSIX `write` सिस्टम कॉल की जानकारी: https://man7.org/linux/man-pages/man2/write.2.html
