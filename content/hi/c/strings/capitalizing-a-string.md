---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:25.721782-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C \u092E\u0947\u0902\
  \ \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\
  \u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947\
  \ \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\
  \u0940 \u0939\u0948 \u0905\u0915\u094D\u0937\u0930 \u092A\u0930\u093F\u0935\u0930\
  \u094D\u0924\u0928 \u0914\u0930 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u091F\u094D\u0930\u0948\u0935\u0930\u094D\u0938\u0932 \u0915\u0940 \u092E\u0942\
  \u0932 \u0938\u092E\u091D \u0915\u0940\u0964 \u091A\u0942\u0902\u0915\u093F C \u092E\
  \u0947\u0902 \u0907\u0938\u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u092C\u093F\
  \u0932\u094D\u091F-\u0907\u0928\u2026"
lastmod: '2024-03-13T22:44:53.110066-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902 \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0915\u094B \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C\
  \ \u0915\u0930\u0928\u0947 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\
  \u093E \u0939\u094B\u0924\u0940 \u0939\u0948 \u0905\u0915\u094D\u0937\u0930 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u0928 \u0914\u0930 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u091F\u094D\u0930\u0948\u0935\u0930\u094D\u0938\u0932\
  \ \u0915\u0940 \u092E\u0942\u0932 \u0938\u092E\u091D \u0915\u0940\u0964 \u091A\u0942\
  \u0902\u0915\u093F C \u092E\u0947\u0902 \u0907\u0938\u0915\u0947 \u0932\u093F\u090F\
  \ \u090F\u0915 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\
  \u0936\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0906\u092A \u0906\u092E\u0924\
  \u094C\u0930 \u092A\u0930 \u092A\u094D\u0930\u0924\u094D\u092F\u0947\u0915 \u0905\
  \u0915\u094D\u0937\u0930 \u0915\u0940 \u091C\u093E\u0902\u091A \u0915\u0930\u0947\
  \u0902\u0917\u0947, \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E\u0928\u0941\
  \u0938\u093E\u0930 \u0909\u0938\u0915\u0947 \u092E\u093E\u092E\u0932\u0947 \u0915\
  \u094B \u0938\u092E\u093E\u092F\u094B\u091C\u093F\u0924 \u0915\u0930\u0947\u0902\
  \u0917\u0947\u0964 \u0928\u0940\u091A\u0947 \u090F\u0915 \u0938\u0930\u0932 \u0915\
  \u093E\u0930\u094D\u092F\u093E\u0928\u094D\u0935\u092F\u0928 \u0939\u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u095B \u0915\u0930\u0928\u093E"
weight: 2
---

## कैसे करें:
C में एक स्ट्रिंग को कैपिटलाइज़ करने की आवश्यकता होती है अक्षर परिवर्तन और स्ट्रिंग ट्रैवर्सल की मूल समझ की। चूंकि C में इसके लिए एक बिल्ट-इन फंक्शन नहीं है, आप आमतौर पर प्रत्येक अक्षर की जांच करेंगे, आवश्यकतानुसार उसके मामले को समायोजित करेंगे। नीचे एक सरल कार्यान्वयन है:

```c
#include <stdio.h>
#include <ctype.h> // islower और toupper फंक्शन्स के लिए

void capitalizeString(char *str) {
    if (str == NULL) return; // सुरक्षा जांच
    
    int capNext = 1; // अगले अक्षर को कैपिटलाइज़ करने के लिए संकेतक 
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // अक्षर को कैपिटलाइज़ करें
            capNext = 0; // संकेतक रीसेट करें
        } else if (str[i] == ' ') {
            capNext = 1; // अगला अक्षर कैपिटलाइज़ किया जाना है
        }
    }
}

int main() {
    char exampleString[] = "hello world. programming in c!";
    capitalizeString(exampleString);
    printf("Capitalized string: %s\n", exampleString);
    return 0;
}
```

नमूना आउटपुट:
```
Capitalized string: Hello World. Programming In C!
```

यह कार्यक्रम `exampleString` स्ट्रिंग को ट्रैवर्स करता है, प्रत्येक अक्षर की जांच करता है कि क्या इसे कैपिटलाइज़ किया जाना चाहिए। `islower` फंक्शन जांचता है कि क्या एक अक्षर एक लोअरकेस अक्षर है, जबकि `toupper` इसे उच्चाक्षर में परिवर्तित करता है। `capNext` संकेतक तय करता है कि क्या मुठभेड़ किया गया अगला अक्षर परिवर्तित किया जाना चाहिए, जब एक स्थान (' ') पाया जाता है, और मूल रूप से स्ट्रिंग के पहले अक्षर को कैपिटलाइज़ करने के लिए सेट किया जाता है।

## गहराई में
दिखाया गया तकनीक सीधी है लेकिन बहुत बड़ी स्ट्रींग्स के लिए या प्रदर्शन-महत्वपूर्ण अनुप्रयोगों में बार-बार निष्पादित होने पर कुशलता से अभाव होता है। ऐतिहासिक और कार्यान्वयन संदर्भों में, C में स्ट्रींग मेनिपुलेशन, कैपिटलाइज़ेशन सहित, अक्सर सीधे बफर मेनिपुलेशन को शामिल करता है, C के निम्न-स्तरीय दृष्टिकोण को प्रतिबिंबित करता है और प्रोग्रामर को स्मृति और प्रदर्शन व्यापारिक बंदों पर पूर्ण नियंत्रण देता है।

स्ट्रिंग्स को कैपिटलाइज़ करने के लिए विकल्पनीय, अधिक सोफिस्टिकेटेड तरीके हैं, विशेष रूप से जब स्थानीयकरण और यूनिकोड अक्षरों को देखते हैं, जहां कैपिटलाइज़ेशन नियम सरल ASCII परिदृश्य से काफी अलग हो सकते हैं। लाइब्रेरीज जैसे कि ICU (International Components for Unicode) इन मामलों के लिए मजबूत समाधान प्रदान करते हैं, लेकिन सभी अनुप्रयोगों के लिए आवश्यक नहीं होने वाली निर्भरताएं और अतिरिक्त लागत सम्मिलित करते हैं।

इसके अतिरिक्त, जबकि प्रदान किया गया उदाहरण C स्टैंडर्ड लाइब्रेरी फंक्शन `islower` और `toupper` का उपयोग करता है, जो `<ctype.h>` का हिस्सा हैं, यह समझना आवश्यक है कि ये ASCII सीमा के भीतर काम करते हैं। यूरोपीय भाषाओं में एक्सेंटेड अक्षरों जैसे ASCII से परे अक्षरों की प्रोसेसिंग की मांग करने वाले अनुप्रयोगों के लिए, सटीक रूप से कैपिटलाइज़ेशन को पूरा करने के लिए अतिरिक्त तर्क या तृतीय-पक्ष लाइब्रेरीज की आवश्यकता होगी।

निष्कर्ष में, जबकि रूपरेखा की गई विधि कई अनुप्रयोगों के लिए उपयुक्त है, सी में मजबूत, अंतर्राष्ट्रीयकृत सॉफ्टवेयर विकसित करने के लिए इसकी सीमाओं और उपलब्ध विकल्पों को समझना महत्वपूर्ण है।
