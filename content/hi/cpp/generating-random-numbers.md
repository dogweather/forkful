---
title:                "कम्प्यूटर प्रोग्रामिंग में रैंडम नंबर उत्पन्न करना"
html_title:           "C++: कम्प्यूटर प्रोग्रामिंग में रैंडम नंबर उत्पन्न करना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में रैंडम नंबर उत्पन्न करना"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Random numbers उपयोगकर्ताओं द्वारा उत्पन्न प्रायः स्वचालित रूप से संचित एक स्रोत हैं। यह स्थान पाने के लिए मूलांकन युक्तियों द्वारा उपयोग होता है। इसका उपयोग पैसे-निर्धारण, डाटा क्रिप्टोग्राफी और सांख्यिकी क्षेत्र में ज्यादातर होता है। 

## कैसे:
```
#include <iostream>
#include <cstdlib>
#include <ctime>
using namespace std;

int main() {
    // random number between 0-9
    int num = rand() % 10;
    cout << "Random number: " << num << endl;

    // seed random number with current time
    srand(time(0));

    // random number between 1-100
    int num2 = rand() % 100 + 1;
    cout << "Another random number: " << num2 << endl;
    return 0;
}
```
आप ```rand()``` और ```srand()```  फंक्शन का उपयोग करके C++ में random numbers उत्पन्न कर सकते हैं। सबसे पहले, आपको ```<cstdlib>``` को शामिल करना होगा जिसमें ```rand()``` फंक्शन होता है। यह random number एक 0-9 के बीच का संख्यात्मक मान देता है। यदि आपने ```srand(0)``` का उपयोग नहीं किया है तो random numbers हमेशा समान स्थानों पर आएंगे। इसलिए, आपके कोड में random numbers को बार-बार उत्पन्न करने के लिए आपको ```rand()``` को अपने कोड में शामिल करके प्रत्येक समय दिया गया समयगणतु से इसे खोलना होगा। 

## गहराई में:
Random numbers की शुरुआत उत्पादन। इसका सबसे पहला अनुप्रयोग रेडियो संप्रदाय के सम्पादक एम.एस. वुडगर्द द्वारा 1907 में प्रस्तुत किया गया था। उस समय से आधुनिक जमीनी संचार मूल्यों को निर्धारित करने के लिए इसका उपयोग होता आया है। और अब, इसका उपयोग विविध क्षेत्रों में किया जाता है जैसे कि डिजिटल क्रिप्टोग्राफी और एक जाँचने युक्त प्रसंस्करणों को प्रदान करने के लिए। अपने पृष्ठभूमि में, random और pseudorandom संख्याओं के बीच अंतर दृश्य होता है। Pseudorandom numbers का उत्पादन एक रुपये के सदस्यों के स्वयंचलित स्रोतों से होता है।

## और पढ़ें:
- [जावा में random numbers](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [Python में random numbers](https://www.programiz.com/python-programming/examples/random-number)
- [Random number generators के अल्टरनेटिव तकनीकों के बारे में](https://www.computerhope.com/jargon/r/randgen.htm)