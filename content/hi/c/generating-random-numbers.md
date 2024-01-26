---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:48:39.611537-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
यादृच्छिक संख्याएँ ऐसी संख्याएँ होती हैं जिनका क्रम उत्पन्न करना अनुमानित नहीं होता। प्रोग्रामर इनका उपयोग गेमिंग, सिमुलेशन, सुरक्षा और टेस्टिंग में करते हैं ताकि परिणाम विविध और अगोचर रहें।

## How to: (कैसे करें:)
C में यादृच्छिक संख्या उत्पन्न करें:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // यादृच्छिकता के लिए सीड इनिशियलाईज़ करें
    srand(time(NULL)); 
    
    // 0 से 99 तक की एक यादृच्छिक संख्या उत्पन्न करें
    int randomNumber = rand() % 100; 
    printf("यादृच्छिक संख्या: %d\n", randomNumber);
    
    return 0;
}
```
सैम्पल आउटपुट:
```
यादृच्छिक संख्या: 42
```

## Deep Dive (गहन अध्ययन)
यादृच्छिक संख्या उत्पन्न करने की पद्धति का इतिहास लंबा है, जिसे पहले परमाणु घड़ियों और प्रकृतिक घटनाओं से लिया जाता था। C में `rand()` और `srand()` फंक्शन्स प्रचलित हैं, जो कि पीएसईयूडीओ-रैंडम (नकली-यादृच्छिक) संख्याएँ देते हैं। हालांकि, जब अधिक सुरक्षा या अनोखे अनियमितता की जरूरत होती है, `/dev/random` जैसे एंट्रॉपी-आधारित स्रोतों की ओर रुख किया जाता है।

## See Also (और भी देखें)
- [Wikipedia: Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [Stack Overflow: How to generate a random number in C?](https://stackoverflow.com/questions/822323/how-to-generate-a-random-number-in-c)
