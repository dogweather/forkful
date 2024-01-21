---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:32:34.492786-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख की तुलना से मतलब है दो अलग-अलग तारीखों को आपस में मिलाना, देखना की कौन सी पहले है या बाद में, या क्या दोनों बराबर हैं। प्रोग्रामर्स इसे करते हैं विभिन्न कार्यों को तारीख के हिसाब से सॉर्ट करने, समय सीमाएं निर्धारित करने, या समय के फासले की गणना करने के लिए।

## How to (कैसे करें):
```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // mktime() converts tm structure to time_t type for comparison
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    if (t1 < t2) {
        return -1; // date1 is earlier than date2
    } else if (t1 > t2) {
        return 1; // date1 is later than date2
    }
    return 0; // dates are equal
}

int main() {
    struct tm date1 = {0, 0, 0, 5, 4, 123}; // 5 May 2023
    struct tm date2 = {0, 0, 0, 15, 4, 123}; // 15 May 2023

    int comparison_result = compare_dates(date1, date2);

    if (comparison_result < 0) {
        printf("Date1 is earlier than Date2\n");
    } else if (comparison_result > 0) {
        printf("Date1 is later than Date2\n");
    } else {
        printf("Dates are equal\n");
    }
    
    return 0;
}
```
Sample Output:
```
Date1 is earlier than Date2
```

## Deep Dive (गहराई से जानकारी):
जब हम C में तारीख की तुलना करते हैं, हमलोग `struct tm` का उपयोग करते हैं, जो समय के तथ्यों (जैसे दिन, महीना, वर्ष) को संजोता है। `mktime()` फंक्शन `struct tm` को `time_t` प्रकार में बदलता है, जो एक लिनियर, लंबा समय प्रतिनिधित्व करता है और तुलना को सरल बनाता है। C में यह क्षमता अपने पूर्वज C89 से आती है। विकल्प में, कुछ लाइब्रेरीज़ जैसे की `date.h` मॉडर्न और सुगम रास्ते प्रदान करती हैं लेकिन स्टैंडर्ड लाइब्रेरी का उपयोग आधिकतर पसंद किया जाता है। यह धारणा कि समय हमेशा लिनियर होता है जटिलताओं का कारण बन सकता है – उदाहरण के लिए, डेलाइट सेविंग्स, लीप सेकंड्स, इत्यादि। फिर भी, `mktime()` उन दिनचर्या स्थितियों को हैंडल करता है।

## See Also (और देखें):
- C Standard Library reference: https://en.cppreference.com/w/c/chrono
- ISO C documentation: https://www.iso.org/standard/74528.html
- "C Programming Absolute Beginner's Guide" by Greg Perry and Dean Miller, for a gentle introduction to C programming.