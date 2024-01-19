---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?(What & Why?)

"मौजूदा तारीख" मिलाने का अर्थ होता है कंप्यूटर के सिस्टम कैलेंडर से वर्तमान तारीख प्राप्त करना। इसे कुछ ब्राउज़र की कुकी सेट करने, लॉग फ़ाइल में टाइमस्टैंप जोड़ने या यूज़र की गतिविधियों की ट्रैकिंग करने जैसे कई कारणों के लिए प्रोग्रामर कर सकते हैं।

## कैसे? (How to?)
C में हम आमतौर पर टाइम लाइब्रेरी का उपयोग करके मौजूदा तारीख मिलता है।

```C
#include <stdio.h>
#include <time.h>
 
int main() {
   time_t t;
   
   time(&t);
   printf("Today's date and time: %s", ctime(&t));
   
   return(0);
}
```
उपरोक्त कोड चलाने पर आपको निम्नलिखित उत्तर मिलेगा:
```
Today's date and time: Sun Jun 13 14:27:55 2023
```

## गहन अध्ययन (Deep Dive)
C प्रोग्रामिंग भाषा के माध्यम से वर्तमान तारीख प्राप्त करने का इतिहास उस के ANSI कार्यक्रम में शुरू हुआ। जब ANSI C को निर्माण किया गया था, केवल उसकी स्तर C लाइब्रेरी में "time.h" लाइब्रेरी शामिल थी। 

वैकल्पिक रूप से, आप POSIX टाइम फ़ंक्शंस का उपयोग कर सकते हैं, जिनमें `strftime()` फ़ंक्शं शामिल है, जिसे कस्टम तारिख़ और समय प्रारूप के लिए उपयोग किया जा सकता है। 

## यह भी देखें (See Also)
लिंक से जुड़े विषय और स्रोत 
1. C प्रोग्रामिंग- मौजूदा टाइम: https://www.tutorialspoint.com/c_standard_library/c_function_time.htm
2. ANSI C और पहले: https://www.learnc.org/
3. विकिपीडिया पर ANSI कार्यक्रम: https://en.wikipedia.org/wiki/ANSI_C.