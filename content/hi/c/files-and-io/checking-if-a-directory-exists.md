---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:55.227476-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C \u092E\u0947\u0902\
  , \u0915\u093F\u0938\u0940 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\
  \u093E \u0915\u0940 \u092E\u094C\u091C\u0942\u0926\u0917\u0940 \u0915\u0940 \u091C\
  \u093E\u0901\u091A `stat` \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u0940 \u091C\u093E\
  \ \u0938\u0915\u0924\u0940 \u0939\u0948, \u091C\u094B \u0928\u093F\u0930\u094D\u0926\
  \u093F\u0937\u094D\u091F \u092A\u0925 \u092A\u0930 \u092B\u093C\u093E\u0907\u0932\
  \ \u092F\u093E \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E \u0915\
  \u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902\u2026"
lastmod: '2024-04-05T21:53:55.130855-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902, \u0915\u093F\u0938\u0940 \u0928\u093F\u0930\u094D\u0926\
  \u0947\u0936\u093F\u0915\u093E \u0915\u0940 \u092E\u094C\u091C\u0942\u0926\u0917\
  \u0940 \u0915\u0940 \u091C\u093E\u0901\u091A `stat` \u092B\u093C\u0902\u0915\u094D\
  \u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\u0940 \u0939\u0948, \u091C\u094B\
  \ \u0928\u093F\u0930\u094D\u0926\u093F\u0937\u094D\u091F \u092A\u0925 \u092A\u0930\
  \ \u092B\u093C\u093E\u0907\u0932 \u092F\u093E \u0928\u093F\u0930\u094D\u0926\u0947\
  \u0936\u093F\u0915\u093E \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902\
  \ \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092A\u094D\u0930\u093E\u092A\u094D\
  \u0924 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 `sys/stat.h` \u0938\u0947 `S_ISDIR`\
  \ \u092E\u0948\u0915\u094D\u0930\u094B \u0915\u093E \u0924\u092C \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0924\
  \u093E\u0915\u093F \u092E\u0942\u0932\u094D\u092F\u093E\u0902\u0915\u0928 \u0915\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0947 \u0915\u093F \u092A\u094D\u0930\
  \u093E\u092A\u094D\u0924 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u090F\u0915\
  \ \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E \u0938\u0947 \u092E\
  \u0947\u0932 \u0916\u093E\u0924\u0940 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\
  \u0902\u0964 \u092F\u0939\u093E\u0901 \u092A\u0930 \u0906\u092A `stat` \u0914\u0930\
  \ `S_ISDIR` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u0915\u0948\u0938\u0947 \u091C\u093E\u0902\u091A \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902 \u0915\u093F \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\
  \u0926\u0947\u0936\u093F\u0915\u093E \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\
  \ \u092F\u093E \u0928\u0939\u0940\u0902."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902, \u091C\u093E\u0902\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे करें:
C में, किसी निर्देशिका की मौजूदगी की जाँच `stat` फ़ंक्शन का उपयोग करके की जा सकती है, जो निर्दिष्ट पथ पर फ़ाइल या निर्देशिका के बारे में जानकारी प्राप्त करता है। `sys/stat.h` से `S_ISDIR` मैक्रो का तब उपयोग किया जाता है ताकि मूल्यांकन किया जा सके कि प्राप्त जानकारी एक निर्देशिका से मेल खाती है या नहीं।

यहाँ पर आप `stat` और `S_ISDIR` का उपयोग करके कैसे जांच कर सकते हैं कि कोई निर्देशिका मौजूद है या नहीं:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // जांचने के लिए निर्देशिका का पथ
    char *dirPath = "/path/to/directory";

    // पथ की स्थिति प्राप्त करें
    int result = stat(dirPath, &stats);

    // जांचें कि निर्देशिका मौजूद है या नहीं
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("निर्देशिका मौजूद है।\n");
    } else {
        printf("निर्देशिका मौजूद नहीं है।\n");
    }

    return 0;
}
```

नमूना उत्तर:
```
निर्देशिका मौजूद है।
```

या, अगर निर्देशिका मौजूद नहीं है:
```
निर्देशिका मौजूद नहीं है।
```

## गहराई से जानकारी:
`stat` संरचना और फ़ंक्शन दशकों से C प्रोग्रामिंग भाषा का हिस्सा रहे हैं, जो Unix से निकले हैं। वे फ़ाइल सिस्टम की जानकारी प्राप्त करने का एक मानकीकृत तरीका प्रदान करते हैं, जो कि भले ही अपेक्षाकृत कम स्तरीय हो, इसकी सादगी और फ़ाइल सिस्टम के मेटाडेटा तक सीधी पहुँच के कारण बहुत अधिक इस्तेमाल किया जाता है।

ऐतिहासिक रूप से, फाइलों और निर्देशिकाओं के अस्तित्व और गुणों की जांच `stat` और इसके व्युत्पन्न (जैसे कि `fstat` और `lstat`) के साथ करना एक सामान्य दृष्टिकोण रहा है। हालांकि, ये फ़ंक्शन सीधे तौर पर ओएस केर्नल के साथ बातचीत करते हैं, जिसमें यदि सही ढंग से संभाला नहीं गया तो अतिरिक्त लागत और संभावित त्रुटियाँ हो सकती हैं।

नई परियोजनाओं के लिए या उच्च-स्तरीय परिदृश्यों में काम करते समय, प्रोग्रामर आधुनिक फ्रेमवर्क या पुस्तकालयों द्वारा प्रदान की गई अधिक अमूर्त फाइल-हैंडलिंग मैकेनिज़्म के लिए विकल्प चुन सकते हैं जो त्रुटियों को अधिक सुचारू रूप से संभालते हैं और एक सरल API प्रदान करते हैं। फिर भी, प्रत्यक्ष फ़ाइल सिस्टम मैनिपुलेशन की आवश्यकता वाले परिदृश्यों में, जैसे कि सिस्टम प्रोग्रामिंग या जब बड़े पुस्तकालयों पर निर्भरता अव्यावहारिक हो, तब `stat` का उपयोग करने की समझ और क्षमता एक मूल्यवान कौशल बनी रहती है।
