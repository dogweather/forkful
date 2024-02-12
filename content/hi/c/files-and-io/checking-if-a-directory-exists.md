---
title:                "डायरेक्टरी मौजूद है या नहीं, जांचना"
date:                  2024-02-03T17:53:55.227476-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं, जांचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में यह जांचना कि कोई निर्देशिका मौजूद है या नहीं, यह फाइल सिस्टम से पूछताछ करने और यह सत्यापन करने में शामिल है कि क्या कोई विशेष पथ एक निर्देशिका की ओर जाता है। प्रोग्रामर अक्सर यह क्रिया इसलिए करते हैं ताकि सुनिश्चित किया जा सके कि फाइल संचालन (जैसे कि फाइलों से पढ़ना या उनमें लिखना) मान्य पथों की ओर निर्देशित हों, जिससे त्रुटियों को रोका जा सके और सॉफ्टवेयर की विश्वसनीयता में वृद्धि हो सके।

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