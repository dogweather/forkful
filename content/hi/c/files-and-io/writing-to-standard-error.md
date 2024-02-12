---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T18:16:38.962442-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में स्टैंडर्ड एरर (त्रुटि) में लिखना यह सुनिश्चित करता है कि त्रुटि संदेश और नैदानिक जानकारी मुख्य प्रोग्राम आउटपुट से अलग धारा में निर्देशित की जाए। प्रोग्रामर इसे करते हैं ताकि त्रुटि संदेशों को स्टैंडर्ड आउटपुट से अलग किया जा सके, जो उन्हें अलग-अलग पढ़ने और प्रोसेस करने में आसान बनाता है, विशेषकर जब प्रोग्रामों के निष्पादन का डीबगिंग या लॉगिंग किया जा रहा हो।

## कैसे करें:

C में, `stderr` धारा का उपयोग त्रुटि संदेश लिखने के लिए किया जाता है। `printf` के साथ स्टैंडर्ड आउटपुट में लिखने के विपरीत, `stderr` में लिखने के लिए `fprintf` या `fputs` का उपयोग किया जा सकता है। यहाँ आप यह कैसे कर सकते हैं:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "यह एक त्रुटि संदेश है।\n");

    fputs("यह एक और त्रुटि संदेश है।\n", stderr);
    
    return 0;
}
```

नमूना आउटपुट (stderr पर):
```
यह एक त्रुटि संदेश है।
यह एक और त्रुटि संदेश है।
```

यह ध्यान देना महत्वपूर्ण है कि जबकि आउटपुट कंसोल में `stdout` के समान प्रतीत होता है, जब टर्मिनल में रीडायरेक्शन का उपयोग किया जाता है, तो अंतर स्पष्ट हो जाता है:

```sh
$ ./your_program > output.txt
```

यह कमांड केवल मानक आउटपुट को `output.txt` में रीडायरेक्ट करता है, जबकि त्रुटि संदेश अभी भी स्क्रीन पर दिखाई देंगे।

## गहराई से 

`stdout` और `stderr` के बीच का अंतर Unix-आधारित सिस्टम्स में C और Unix के शुरुआती दिनों से ही है। यह विभाजन अधिक रॉबस्ट त्रुटि हैंडलिंग और लॉगिंग की अनुमति देता है, क्योंकि यह प्रोग्रामरों को मानक प्रोग्राम आउटपुट से स्वतंत्र रूप से त्रुटि संदेशों को रीडायरेक्ट करने की क्षमता प्रदान करता है। जबकि `stderr` मूल रूप से अनबफर्ड है ताकि त्रुटि संदेशों का तत्काल आउटपुट सुनिश्चित किया जा सके, जो दुर्घटनाओं और अन्य महत्वपूर्ण मुद्दों के समय में डीबगिंग में मदद करता है, `stdout` आम तौर पर बफर्ड होता है, जिसका मतलब है कि इसका आउटपुट बफर फ्लश होने तक में देरी हो सकती है (जैसे, प्रोग्राम पूरा होने पर या मैन्युअल फ्लशिंग)।

आधुनिक अनुप्रयोगों में, `stderr` में लिखना अभी भी प्रासंगिक है, विशेष रूप से कमांड-लाइन टूल्स और सर्वर अनुप्रयोगों के लिए जहाँ नियमित लॉग संदेशों और त्रुटियों के बीच भेद करना महत्वपूर्ण है। हालांकि, अधिक जटिल त्रुटि हैंडलिंग के लिए, विशेष रूप से GUI अनुप्रयोगों में या जहाँ अधिक सोफिस्टिकेटेड लॉगिंग मैकेनिज्म की आवश्यकता होती है, प्रोग्रामर समर्पित लॉगिंग लाइब्रेरीज का उपयोग कर सकते हैं जो संदेशों के प्रारूप, गंतव्यों (जैसे, फाइलें, नेटवर्क) और गंभीरता स्तरों (जानकारी, चेतावनी, त्रुटि, आदि) पर अधिक नियंत्रण प्रदान करती हैं।

`stderr` C में त्रुटि रिपोर्टिंग के लिए एक मौलिक तंत्र प्रदान करता है, लेकिन प्रोग्रामिंग प्रथाओं का विकास और उन्नत लॉगिंग फ्रेमवर्क की उपलब्धता का मतलब है कि यह अक्सर आधुनिक त्रुटि हैंडलिंग रणनीतियों के लिए केवल एक शुरुआती बिंदु है।