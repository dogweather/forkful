---
title:                "उपवाक्यांश निष्कर्षित करना"
date:                  2024-02-03T17:58:02.436096-07:00
model:                 gpt-4-0125-preview
simple_title:         "उपवाक्यांश निष्कर्षित करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/extracting-substrings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

C में सबस्ट्रिंग्स को निकालना एक बड़ी स्ट्रिंग से आधारित मापदंड, जैसे कि स्थिति और लंबाई के आधार पर एक छोटी स्ट्रिंग (सबस्ट्रिंग) बनाने की प्रक्रिया है। प्रोग्रामर अक्सर इस कार्य को पाठ पार्सिंग, डेटा प्रोसेसिंग, या इनपुट सत्यापन के लिए करते हैं, जो पाठ डेटा को कुशलतापूर्वक संभालने और विश्लेषित करने में एक महत्वपूर्ण कौशल बनाता है।

## कैसे:

कुछ उच्च-स्तरीय भाषाओं के विपरीत जो सबस्ट्रिंग निष्कर्षण के लिए निर्मित विधियाँ प्रदान करती हैं, C में इसकी स्ट्रिंग हेरफेर फ़ंक्शन का उपयोग करके एक अधिक मैन्युअल दृष्टिकोण की आवश्यकता होती है। यहाँ C में प्रभावी रूप से एक सबस्ट्रिंग कैसे निकाली जाती है:

### उदाहरण 1: `strncpy` का उपयोग करना

```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hello, World!";
    char buffer[20];

    // "Hello, World!" से "World" निकालें
    strncpy(buffer, text + 7, 5);
    buffer[5] = '\0'; // नल-समाप्ति सुनिश्चित करें

    printf("निकाला गया सबस्ट्रिंग: %s\n", buffer);
    // आउटपुट: निकाला गया सबस्ट्रिंग: World
    return 0;
}
```

### उदाहरण 2: एक फंक्शन बनाना

बार-बार उपयोग के लिए, सबस्ट्रिंग्स को निकालने के लिए एक समर्पित फंक्शन अधिक कुशल हो सकता है:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *source, int from, int n, char *target) {
    strncpy(target, source + from, n);
    target[n] = '\0'; // नल-समाप्ति सुनिश्चित करें
}

int main() {
    char text[] = "Programming in C";
    char buffer[50];

    extractSubstring(text, 0, 11, buffer);
    printf("निकाला गया सबस्ट्रिंग: %s\n", buffer);
    // आउटपुट: निकाला गया सबस्ट्रिंग: Programming
    return 0;
}
```

## गहराई में जानकारी

C में सबस्ट्रिंग्स को निकालना मुख्यतः पॉइंटर मैनिपुलेशन और सावधानीपूर्वक स्मृति प्रबंधन के माध्यम से संभाला जाता है, जो डेटा को संभालने के लिए भाषा के निम्न-स्तरीय दृष्टिकोण को दर्शाता है। यह विधि सी प्रोग्रामिंग के प्रारंभिक दिनों से डेट है जब सीमित कंप्यूटिंग शक्ति के कारण संसाधनों का किफायती प्रबंधन महत्वपूर्ण था। जबकि निर्मित सबस्ट्रिंग फ़ंक्शन की अनुपस्थिति को एक जिम्मेदारी माना जा सकता है, यह C के दर्शन को दर्शाता है जो प्रोग्रामरों को स्मृति प्रबंधन पर पूर्ण नियंत्रण प्रदान करता है, अक्सर अनुकूलित लेकिन अधिक जटिल कोड की ओर ले जाता है।

आधुनिक प्रोग्रामिंग के क्षेत्र में, Python और JavaScript जैसी भाषाएँ `slice()` या इंडेक्स का उपयोग करके स्ट्रिंग स्लाइसिंग के लिए निर्मित विधियाँ प्रदान करती हैं। ये उच्च-स्तरीय भाषाएँ पृष्ठभूमि में स्मृति प्रबंधन को संभालती हैं, उपयोग में आसानी और पठनीयता के लिए कुछ हद तक नियंत्रण का व्यापार करती हैं।

C प्रोग्रामर्स के लिए, पॉइंटर अंकगणित और स्मृति आवंटन की समझ जैसे कार्यों के लिए महत्वपूर्ण है। यद्यपि यह दृष्टिकोण स्ट्रिंग्स को स्मृति में कैसे प्रतिनिधित्व किया जाता है और हेरफेर किया जाता है, इसकी गहरी समझ की आवश्यकता है, यह नियंत्रण और कुशलता की अप्रतिम पेशकश करता है, C प्रोग्रामिंग की हॉलमार्क विशेषताएँ जिन्होंने इसे दशकों से प्रदर्शन-महत्वपूर्ण अनुप्रयोगों में प्रासंगिक रखा है। हालांकि, उन लोगों के लिए जो उच्च-स्तरीय अनुप्रयोगों पर काम कर रहे हैं जहां सीधे स्मृति प्रबंधन कम चिंता का विषय है, निर्मित सबस्ट्रिंग कार्यक्षमताओं वाली भाषाएँ एक अधिक सीधा और कम त्रुटि-प्रवण दृष्टिकोण प्रदान कर सकती हैं।