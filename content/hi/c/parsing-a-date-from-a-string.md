---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:35:25.908497-07:00
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पार्सिंग डेट फ्रॉम ए स्ट्रिंग का मतलब है कि एक टेक्स्ट स्ट्रिंग से डेट और टाइम की जानकारी निकालना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि बहुत सी एप्लिकेशंस में डेटा को सही फॉर्मेट में स्टोर और प्रोसेस करने की जरूरत होती है। 

## How to: (कैसे करें:)
```c
#include <stdio.h>
#include <time.h>

int main() {
    const char *dateStr = "01/04/2023";
    struct tm tm;
    if (strptime(dateStr, "%d/%m/%Y", &tm)) {
        printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Date parsing failed!\n");
    }
    return 0;
}
```
उपरोक्त कोड एक स्ट्रिंग से डेट को पार्स करके सही फॉर्मेट में प्रिंट करेगा।
सैंपल आउटपुट: 
```
Year: 2023, Month: 4, Day: 1
```

## Deep Dive (गहराई से समझे)
समय के साथ, डेट पार्सिंग ने विकास किया है। पुराने समय में प्रोग्रामर्स मैन्युअली पार्सिंग करते थे, पर अब लाइब्रेरीज जैसे कि `<time.h>` में `strptime` फंक्शन इसे बहुत आसान बना देते हैं। वैकल्पिक रूप से, आप `sscanf` या तीसरे पक्ष की बाहरी लाइब्रेरीज का भी उपयोग कर सकते हैं। प्रत्येक मेथड के अपने फायदे और नुकसान हैं। C11 और C18 के नए संस्करणों में सुरक्षित और सुविधाजनक फंक्शंस का समावेश किया गया है।

## See Also (यह भी देखें)
- C Standard Library `<time.h>` documentation: https://en.cppreference.com/w/c/chrono
- C11 Standard: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
- ISO/IEC 9899:2018 (C18) at ISO: https://www.iso.org/standard/74528.html
