---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-02-03T18:06:12.254348-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में एक टेक्स्ट फाइल को पढ़ना आपके सिस्टम पर एक फाइल को खोलने और जानकारी निकालने तथा जरूरत के अनुसार उसे संशोधित या प्रदर्शित करने की प्रक्रिया शामिल है। प्रोग्रामर अक्सर कॉन्फ़िगरेशन फ़ाइलों को प्रोसेस करने, प्रोसेसिंग के लिए इनपुट पढ़ने, या फ़ाइल फॉर्मेट में संग्रहीत डेटा का विश्लेषण करने के लिए ऐसा करते हैं, जो एप्लिकेशनों में लचीलापन और बढ़ी हुई कार्यक्षमता की अनुमति देता है।

## कैसे:

C में एक टेक्स्ट फ़ाइल पढ़ना शुरू करने के लिए, आप मुख्य रूप से मानक I/O पुस्तकालय से `fopen()`, `fgets()`, और `fclose()` फ़ंक्शन्स के साथ काम करते हैं। यहाँ एक सरल उदाहरण है जो `example.txt` नामक फाइल को पढ़ता है और उसकी सामग्री को मानक आउटपुट पर प्रिंट करता है:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *filePointer;
    char buffer[255]; // पाठ पंक्तियों को संग्रहीत करने के लिए बफर

    // फ़ाइल को पढ़ने के मोड में खोलें
    filePointer = fopen("example.txt", "r");

    // यदि फाइल सफलतापूर्वक खुली नहीं तो जाँच करें
    if (filePointer == NULL) {
        printf("फाइल खोलने में सक्षम नहीं हुआ। \n");
        return 1;
    }

    while (fgets(buffer, 255, filePointer) != NULL) {
        printf("%s", buffer);
    }

    // संसाधन मुक्त करने के लिए फ़ाइल बंद करें
    fclose(filePointer);
    return 0;
}
```

मान लें `example.txt` में शामिल है:
```
नमस्ते, दुनिया!
सी प्रोग्रामिंग में आपका स्वागत है।
```

आउटपुट होगा:
```
नमस्ते, दुनिया!
सी प्रोग्रामिंग में आपका स्वागत है।
```

## गहराई में:

C में फाइलें पढ़ने का एक समृद्ध इतिहास है, जो यूनिक्स के प्रारंभिक दिनों की ओर इशारा करता है जब टेक्स्ट स्ट्रीम्स की सादगी और सुंदरता मौलिक थी। इससे पूर्णता, हिसाब-किताब, लॉगिंग और अंतर-प्रक्रिया संचार सहित अनेक उद्देश्यों के लिए टेक्स्ट फाइलों की अपनाई गई। C भाषा के फ़ाइल I/O पुस्तकालय की सादगी, जो `fopen()`, `fgets()`, और `fclose()` जैसे फ़ंक्शन्स द्वारा उदाहरित है, इसके डिज़ाइन दर्शन को रेखांकित करती है जो बुनियादी उपकरण प्रदान करता है जिनका उपयोग प्रोग्रामर जटिल प्रणालियों का निर्माण कर सकते हैं।

इतिहासिक रूप से, जबकि ये फंक्शन्स अनगिनत एप्लिकेशनों के लिए सेवा प्रदान कर चुके हैं, आधुनिक प्रोग्रामिंग प्रथाओं ने कुछ सीमाओं पर प्रकाश डाला है, विशेषकर एरर हैंडलिंग, फाइल ए�