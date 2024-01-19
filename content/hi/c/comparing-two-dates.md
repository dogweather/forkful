---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

---
title: दो तारीखों की तुलना: सी प्रोग्रामिंग में 

## क्या और क्यों? 
तारीखों की तुलना होती है दो तारीखों को मिलाकर देखने उनमें अंतर समझने की प्रक्रिया। प्रोग्रामर्स इसे तारीखों के क्रमबद्धता और अंतरालों को समझने के लिए करते हैं। 

## कैसे करें:
यहाँ एक सरल सी प्रोग्राम है:

```C
#include <stdio.h>
#include <time.h>

int main(){
    time_t now;
    time(&now);

    struct tm *local = localtime(&now);

    // प्रिंट करे यहाँ UTC time 
    printf("Date: %02d-%02d-%04d\n", local->tm_mday, local->tm_mon + 1, (local->tm_year + 1900));

    return 0;
}
```

आपकी आउटपुट:

```C
Date: 24-01-2023
```

## गहराई में:
ऐतिहासिकता: `time.h` है एक सी लाइब्रेरी कि तारीख और समय से सम्बंधित फ़ंक्शन्स का समर्थन करती है। यह 1978 में ANSI C स्टैंडर्ड के भाग के रूप में शामिल हुई थी। 

विकल्प: कई अन्य लिब्र