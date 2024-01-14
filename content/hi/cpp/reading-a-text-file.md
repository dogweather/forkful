---
title:    "C++: एक पाठ फ़ाइल पढ़ना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें किसी टेक्स्ट फ़ाइल को पढ़ने की आवश्यकता होती है, जैसे कि प्रोग्राम को कोई डेटा फ़ाइल से रैन्डम नम्बर्स लेने के लिए या एक विशेष स्ट्रिंग की सेटिंग्स को रिएड करने के लिए। इसलिए, यह उदाहरण सीखने और समझने के लिए एक अच्छा माध्यम हो सकता है।

## कैसे करें

एक टेक्स्ट फ़ाइल पढ़ने के लिए, हम स्ट्रिंग समेत एक सरणी को डिक्लेयर करते हैं और `ifstream` ऑब्जेक्ट का उपयोग करके फ़ाइल को ओपन करते हैं। तब आप `getline()` फ़ंक्शन का उपयोग करके लाइन एक बार में पढ़ सकते हैं और उसे स्ट्रिंग में स्टोर कर सकते हैं। आप ऋण से पाए गए डेटा को इस तरह से उपयोग कर सकते हैं जैसे कि स्पेसिफ़ाइंड स्ट्रिंग के लिए खोज करने के लिए।

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    string data[10]; // सरणी डिक्लेयर करें
    ifstream file; // फाइल ओपन करें
    file.open("data.txt"); // फाइल को ओपन करें
    int i = 0;
    while (getline(file, data[i])) // फाइल से लाइन पढ़ें और स्ट्रिंग में स्टोर करें
    {
        i++;
    }
    cout << "File contents:" << endl;
    for (int j = 0; j < i; j++)
    {
        cout << data[j] << endl; // डेटा प्रिंट करें
    }
    file.close(); // फाइल को क्लोज करें
    return 0;
}
```

यदि हम `data.txt` फ़ाइल को देखें, तो इसमें निम्न डेटा हो सकता है:

```
This is a sample text file
for learning purposes
```

और अब हमारा आउटपुट हो सकता है:

```
File contents:
This is a sample text file
for learning purposes
```

## गहराई में समझें

जब आप एक फ़ाइल को पढ़