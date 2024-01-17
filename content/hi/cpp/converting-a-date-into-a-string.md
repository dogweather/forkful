---
title:                "एक तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "C++: एक तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डेट को स्ट्रिंग में रूपांतरण करना एक प्रोग्रामर का एक आम काम है। इसका कारण यह है कि डेटा ऑपरेशन्स में डेटा को स्ट्रिंग में रूपांतरित करना बहुत सरल होता है और प्रोग्राम को और अधिक न्यूनतम बनाता है।

## कैसे:
```C++
// तारीख के एक उदाहरण को स्ट्रिंग में रूपांतरित करें
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // इस कोड में हम वर्तमान तिथि को स्ट्रिंग में रूपांतरित कर रहे हैं
    time_t now = time(0);
    char* date = ctime(&now);
    cout << "वर्तमान तिथि अभिलेख: " << date << endl;
    
    // अन्य उदाहरण भी कर सकते हैं
    // जैसे तारीख की त्रिप्ति (epoch) का पता लगाएं
    time_t epoch = now - 1970 * (60 * 60 * 24); // 1970 से तारीख त्रिप्ति तक आने के दिन
    cout << "तारीख त्रिप्ति: " << epoch << endl;
    
    return 0;
}
```
उत्पाद दिखाएँ:
```
वर्तमान तिथि अभिलेख: Thu Sep 30 18:00:04 2021
तारीख का त्रिप्ति: 18829 // वर्तमान तिथि से पिछले क्रम से गणना की तारीख त्रिप्ति
```

## गहराई खोज करें:
- आर्क कैसे स्ट्रिंग में कनवर्ट करें
- अन्य प्रोग्रामिंग भाषाओं में डेट स्ट्रिंग कनवर्ट करने के विकल्प
- डेट स्ट्रिंग कनवर्ट करने के पीछे के तकनीकी अंतर्निर्णय

## और देखें:
- [strftime फंक्शन के लिए C++ डॉक्यूमेंटेशन](https://www.cplusplus.com/reference/ctime/strftime/)
- [कैसे वर्तमान तिथि को स्ट्रिंग में कनवर्ट करें (स्टैक ओवरफ्लो)](https://stackoverflow.com/questions/17353/how-do-i-convert-current-date-in-to-a-string/17357#17357)
- [कैसे विभिन्न भाषाओं में डेट स्ट्रिंग कनवर्ट करने के विकल्प (गिथब)](https://github.com/globalizejs/globalize/blob/master/doc/api/date/date-formatter.md)