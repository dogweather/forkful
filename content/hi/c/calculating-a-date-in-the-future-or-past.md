---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "C: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत की तारीख की गणना का मतलब है किसी विशेष दिनांक के कुछ समय बाद या पहले की तारीख का पता लगाना। प्रोग्रामर्स इसे करते हैं क्योंकि यह विभिन्न एप्लिकेशन्स, जैसे की कैलेंडर ऐप्स या समय-संबंधी डाटा की संगठनात्मक आवश्यकताओं में मदद करता है। 

## कैसे करें:

यहां कुछ साधारण कोड उदाहरण और आउटपुट दिए गए हैं। 

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm newdate;
    char buf1[150];

    time(&now);
    newdate = *localtime(&now);

    newdate.tm_mday += 7;    // here we're adding a week to the current date
    mktime(&newdate);

    strftime(buf1, sizeof(buf1), "%Y-%m-%d\n", &newdate);

    printf("After a week, the date will be: %s\n", buf1);

    return 0;
}
```

इस कोड का आउटपुट जो हमें एक सप्ताह बाद की तारीख दे रहा होगा, कुछ इस तरह होगा:

```
After a week, the date will be: 2022-03-17
```

## गहरी खुदाई:

भविष्य या अतीत की तारीख की गणना का अभ्यास Unix आपरेटिंग सिस्टम के समय से ही किया जा रहा है। इसके बदले `chrono` जैसे modern C++ libraries भी उपलब्ध हैं जो इसे और आसान बना देते हैं। आमतौर पर C के `tm` structure का upyog किया जाता है जिसमें विभिन्न fields (जैसे - `tm_mday`, `tm_mon` आदि) होती हैं, अपनी सुविधानुसार महीनों, दिनों या वर्षों को जोड़ने या निकालने के लिए। 

## अधिक जानकारी के लिए:

2. [Adding days to date in C](https://stackoverflow.com/questions/25010159/adding-days-to-date-in-c)