---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डेट/तारीख को स्ट्रिंग में कन्वर्ट करना, इसका मतलब है किसी विशिष्ट डेट/तारीख को पाठ्य रूप में दर्शाना। प्रोग्रामर इसे सरलता और उचित प्रस्तुति के लिए करते हैं।

## कैसे:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm now = *localtime(&t);

    char date[20];
    sprintf(date, "%02d-%02d-%02d", now.tm_mday, now.tm_mon + 1, now.tm_year + 1900);
    printf("%s", date);

    return 0;
}
```

उपरोक्त कोड की आउटपुट:

```C
20-06-2022
```

## गहराई में:

### ऐतिहासिक संदर्भ:
C Language में डेट/तारीख को स्ट्रिंग में कन्वर्ट करने का काम, अधिकारिक तौर पर `time.h` लाइब्रेरी का उपयोग करके होता है, जो 1970 के दशक के आरंभ में ANSI C में जोड़ी गई थी।

### वैकल्पिक:
अल्गोरिद्म में `strftime` फ़ंक्शन का उपयोग भी किया जा सकता है, जो एक और विकसित तारीख/समय स्थानांतरण तरीका है।

### आवेदन विवरण:
यह प्रक्रिया समय को एक `tm` संरचना में जोड़ने और फिर उसे एक विशिष्ट स्ट्रिंग फ़ॉर्मेट में बदलने पर आधारित है। 

## यह भी देखें:

1. [सी प्रोग्रामिंग दिनांक और समय - tutorialspoint](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
2. [C Library - <time.h> - tutorialspoint](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
3. [Date string in C programming - StackOverflow](https://stackoverflow.com/questions/1442116/how-to-get-date-and-time-value-in-c-program)