---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

`stderr` से आप कम्प्यूटर प्रोग्राम में गलतियां और लॉग्स दिखा सकते हैं। इससे प्रोग्रामर एरर सन्देश और डिबग जानकारी को सामान्य आउटपुट से अलग कर सकते हैं।

## कैसे करें:

```C++
#include <iostream>

int main() {
    // सामान्य मैसेज
    std::cout << "सब कुछ ठीक है।" << std::endl;

    // एरर मैसेज
    std::cerr << "यहाँ कुछ गलत हो गया है।" << std::endl;

    return 0;
}
```

सैंपल आउटपुट:
```
सब कुछ ठीक है।
यहाँ कुछ गलत हो गया है।
```

## गहराई में जानें:

`stderr` का इस्तेमाल शुरू से यूनिक्स लाइक सिस्टम्स में होता आया है। यह सीधे कंसोल पर जाता है, फाइल में नहीं जब तक उसे रिडायरेक्ट नहीं किया जाता। वैकल्पिक रूप से, लॉगिंग फ्रेमवर्क्स या फाइल सिस्टम्स का इस्तेमाल हो सकता है, लेकिन `stderr` का अपना एक स्टैण्डर्ड महत्त्व है। जब `cerr` का उपयोग होता है, तो बफर नहीं होता और मैसेज तुरंत दिखाई देता है।

## संबंधित सूत्र:

- C++ में I/O के बारे में मूल जानकारी: http://www.cplusplus.com/doc/tutorial/basic_io/
- स्ट्रीम्स में बफरिंग और उनके प्रभाव: https://en.cppreference.com/w/cpp/io/basic_ios/rdbuf
- लॉगिंग बेस्ट प्रैक्टिसेज: https://www.hindiblogger.com/logging-best-practices-in-programming/
