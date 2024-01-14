---
title:                "C++: एक तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

C++ में तारीख को स्ट्रिंग में रूपांतरित करने का काम क्यों किया जाता है? अक्सर तारीख को स्ट्रिंग में रूपांतरित करने की जरूरत होती है जब हम अपने प्रोग्राम में तारीख और समय को प्रिंट करना चाहते हैं, जिससे प्रयोगकर्ता को समय को आसानी से समझने में मदद मिले।

## कैसे करें

यदि आप अपने C++ प्रोग्राम में तारीख को स्ट्रिंग में रूपांतरित करना चाहते हैं, तो आपको `#include <ctime>` दिया गया बिल्ट-इन लाइब्रेरी इस्तेमाल करनी होगी। यहां हम `ctime` को इस्तेमाल करके तारीख को स्ट्रिंग में रूपांतरित करने के दो एक्साम्पल देखेंगे।

```C++
// Example 1: भारत में आज की तारीख को string में रूपांतरित करें
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // वर्तमान समय को पाने के लिए समय का ऑब्जेक्ट बनाएं
    time_t now = time(0);

    // time ऑब्जेक्ट को localtime से बदलें
    tm* localTime = localtime(&now);

    // तारीख को स्ट्रिंग में रूपांतरित करें
    char date_str[50];
    strftime(date_str, 50, "%d/%m/%Y", localTime);

    // प्रिंट करें
    cout << "आज की तारीख: " << date_str << endl;
    return 0;
}
/* Output:
आज की तारीख: 22/10/2021
*/

// Example 2: एक बार में कई समय जोड़ें
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // तारीख को स्ट्रिंग में रूपांतरित करें
    char date_str[50];
    strftime(date_str, 50, "%d/%m/%Y, %H:%M:%S, भारतीय मानक समय", localTime);

    // प्रिंट करें
    cout << "तारीख और समय: " << date_str << endl;
    return 0;
}
/* Output:
तारीख और समय: 22/10/2021, 23:08:33, भारतीय मानक समय
*/

``