---
title:                "C++: दो तिथियों की तुलना करना"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
क्या आपको कभी अपनी दस्तावेज़ों में दो तारीखों की तुलना करनी पड़ी है? शायद आपको मूल्यांकन करने के लिए सही समय का अनुमान लगाना था? या फिर आप सिर्फ दो तिथियों के बीच कितने दिनों का अंतर है जानना चाहते थे? चाहे आपका मुद्दा कोडिंग से संबंधित हो या दस्तावेज़ों को सरल रखने के लिए, दो तारीखों की तुलना सीखने का महत्व हो सकता है। इस ब्लॉग पोस्ट में हम दो तारीखों को तुलना करने के तरीकों पर विस्तार से चर्चा करेंगे।

## कैसे
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // दो तारीखों को सेट करें
    time_t t1 = time(0); // वर्तमान समय लें
    tm *t2 = new tm();
    t2->tm_year = 2020 - 1900; // साल
    t2->tm_mon = 03 - 1; // महीना
    t2->tm_mday = 08; // दिन

    // दोनों तारीखों को समान करें
    tm *date1 = localtime(&t1);
    mktime(date1);
    mktime(t2);

    // तारीखों का अंतर निकालें
    int difference = date1->tm_mday - t2->tm_mday;

    // अंतर को आउटपुट करें
    cout << "तारीखों का अंतर है: " << difference << " दिन" << endl;

    // दुर्लभ तारीख से दो तारीखों का अंतर निकालें
    tm *t3 = new tm();
    t3->tm_year = 2020 - 1900;
    t3->tm_mon = 03 - 1;
    t3->tm_mday = 29;
    mktime(t3);
    int difference2 = t3->tm_mday - t2->tm_mday;
    cout << "तारीखों का अंतर है: " << difference2 << " दिन" << endl;

    return 0;
}
```
उपरोक्त कोड सिस्टम के स्क्रीनशॉट इंजीनियरिंग करें जो दो तारीखों को मिलाकर अंतर को प्रिंट करेगा। आउटपुट स्क्रीनशॉट मोबाइल फ्रेम में प्रदर