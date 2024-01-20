---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

Title: दो तारीखों की तुलना C++ (वर्तमान संस्करण) में 

## क्या और क्यों? (What & Why?)

तारीखों की तुलना का मतलब होता है कि हम देखते हैं कि कौन सी तारीख अन्य की तुलना में पहले या बाद में है। कोडर्स इसका उपयोग यह निर्धारित करने के लिए करते हैं कि क्या एक घटना पहले हुई है या एक निर्दिष्ट समय के बाद।

## कैसे: (How to:)

```C++
#include <iostream>
#include<chrono>
using namespace std;
int main() {
   auto current_time = std::chrono::system_clock::now();
   auto one_hour_later = current_time + std::chrono::hours(1);
   if (current_time < one_hour_later) {
      cout << "One hour later is in future!" << endl;
   }
   return 0;
}

```
उत्पादन संपादक 
```
One hour later is in future!
```
## गहन समझ (Deep Dive)

1. ऐतिहासिक सन्दर्भ: C++ में तारीखों की तुलना को संनिवेश करने के लिए std::chrono लाइब्ररी का उपयोग किया जाता है, जो C++11 के साथ शामिल हुई थी।
2. विकल्प: अन्य भाषा जैसे कि Python, Java इत्यादि कर्नल (core) तारीख और समय का समर्थन करते हैं।
3. आधार विवरण: std::chrono::system_clock::now वर्तमान समय प्रदान करता है। std::chrono::hours(1) आने वाले समय तारीख जोड़ता है। 

## देखने के लिए (See Also)

1. C++ रेफरेन्स किताब- std::chrono लाइब्ररी: https://en.cppreference.com/w/cpp/chrono
2. तारीख़ और समय से क्या क्या कर सकते हैं उसके लिए यहाँ क्लिक करें: https://www.cplusplus.com/reference/ctime/