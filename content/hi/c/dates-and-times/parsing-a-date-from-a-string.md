---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
date:                  2024-02-03T18:06:51.862018-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में एक स्ट्रिंग से तारीख को पार्स करना तारीख के पाठ्य प्रतिनिधित्व को एक ऐसे प्रारूप में बदलना शामिल है जिसे प्रोग्राम अधिक प्रभावी ढंग से संभालने और विश्लेषण कर सकें। यह दिनांक अंकगणित, तुलना, और विभिन्न स्थानों के लिए प्रारूपण जैसे कार्यों के लिए महत्त्वपूर्ण है, क्योंकि यह प्रोग्रामरों को उपयोगकर्ता इनपुट या डेटासेट प्रविष्टियों को मानकीकृत तरीके से संभालने की अनुमति देता है।

## कैसे:

C सीधे तौर पर स्ट्रिंग्स से तारीखों को पार्स करने का निर्मित तरीका प्रदान नहीं करता, इसलिए हम अक्सर POSIX सिस्टम के लिए `<time.h>` लाइब्रेरी में उपलब्ध `strptime` फ़ंक्शन का सहारा लेते हैं। यह फ़ंक्शन हमें इनपुट स्ट्रिंग के अपेक्षित प्रारूप को निर्दिष्ट करने और इसे एक `struct tm` में पार्स करने की अनुमति देता है, जो कि घटकों में टूटी हुई कैलेंडर दिनांक और समय का प्रतिनिधित्व करता है।

यहाँ एक सरल उदाहरण है कि कैसे एक स्ट्रिंग से तारीख को पार्स करने के लिए `strptime` का उपयोग करें:

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // स्ट्रक्चर tm में दिनांक स्ट्रिंग को पार्स करना
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("Failed to parse date.\n");
    } else {
        // strftime का उपयोग करके पढ़ने योग्य प्रारूप में तारीख को प्रिंट करना
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("Parsed date: %s\n", buf);
    }

    return 0;
}
```

इस कार्यक्रम के लिए नमूना आउटपुट होगा:

```
Parsed date: Saturday, April 01, 2023
```

`sट्र्पटाइम` पैटर्न से मेल नहीं खाने या अप्रत्याशित इनपुट का सामना करने जैसी संभावित त्रुटियों को संभालना अत्यंत महत्वपूर्ण है।

## गहराई से समझ:

`strptime` फ़ंक्शन, जबकि शक्तिशाली, मानक C लाइब्रेरी का हिस्सा नहीं है और मुख्यतः POSIX-अनुरूप सिस्टम जैसे कि Linux और UNIX पर पाया जाता है। यह सीमा दर्शाती है कि `strptime` का उपयोग करके स्ट्रिंग्स से तारीखों को पार्स करने वाले प्रोग्राम अतिरिक्त संगतता परतों या लाइब्रेरियों के बिना non-POSIX सिस्टम जैसे कि Windows पर पोर्टेबल नहीं हो सकते।

इतिहासिक रूप से, C में दिनांकों और समयों को संभालने की आवश्यकता बहुत अधिक मैन्युअल मैनिप्युलेशन और ध्यान रखने की आवश्यकता थी, विशेष रूप से विभिन्न स्थानों और समय क्षेत्रों को ध्यान में रखते हुए। C++ `<chrono>` लाइब्रेरी और हॉवर्ड हिनैंट की C++ के लिए डेट लाइब्रेरी जैसे C के आधुनिक विकल्प और विस्तार, पार्सिंग सहित दिनांक और समय मैनिपुलेशन के लिए अधिक मजबूत समाधान प्रदान करते हैं। ये लाइब्रेरियां आमतौर पर विभिन्न दिनांक प्रारूपों, समय क्षेत्रों, और त्रुटि संभालने की तंत्र के लिए बेहतर समर्थन प्रदान करती हैं, जिससे वे व्यापक दिनांक और समय मैनिपुलेशन क्षमताओं की आवश्यकता वाली नई परियोजनाओं के लिए पसंदीदा होती हैं।

फिर भी, C में स्ट्रिंग्स से दिनांकों को पार्स करने का तरीका समझना लाभकारी हो सकता है, विशेष रूप से उन परियोजनाओं पर काम करते समय या उन्हें बनाए रखते समय जिन्हें उन सिस्टमों के साथ संगत होने की आवश्यकता होती है जहां ये आधुनिक उपकरण उपलब्ध नहीं होते हैं या जब सख्त C प्रोग्रामिंग वातावरण की सीमाओं के भीतर काम किया जाता है।