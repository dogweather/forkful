---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:47.586487-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C \u092E\u0947\u0902\
  , \u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\u0930\
  \u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\u092C\u0938\u0947\
  \ \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\u0940\u0915\u093E \u0938\
  \u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 I/O \u0932\u093E\u0907\u092C\u094D\
  \u0930\u0947\u0930\u0940 \u0938\u0947 `printf` \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u0939\
  \u0948\u0964 `printf` \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0938\u094D\u091F\
  \u0948\u0902\u0921\u0930\u094D\u0921\u2026"
lastmod: '2024-03-13T22:44:53.147909-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902, \u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\
  \u091F \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u0947 \u0915\u093E\
  \ \u0938\u092C\u0938\u0947 \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\
  \u0940\u0915\u093E \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 I/O \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0938\u0947 `printf` \u092B\u093C\
  \u0902\u0915\u094D\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E \u0939\u0948\u0964 `printf` \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0928 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0906\u0909\u091F\
  \u092A\u0941\u091F \u0921\u093F\u0935\u093E\u0907\u0938, \u0906\u092E\u0924\u094C\
  \u0930 \u092A\u0930 \u0938\u094D\u0915\u094D\u0930\u0940\u0928 \u092A\u0930 \u0938\
  \u094D\u0935\u0930\u0942\u092A\u093F\u0924 \u0906\u0909\u091F\u092A\u0941\u091F\
  \ \u0915\u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u0930\u0932 \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## कैसे करें:
C में, डिबग आउटपुट प्रिंट करने का सबसे सामान्य तरीका स्टैंडर्ड I/O लाइब्रेरी से `printf` फ़ंक्शन का उपयोग करना है। `printf` फ़ंक्शन स्टैंडर्ड आउटपुट डिवाइस, आमतौर पर स्क्रीन पर स्वरूपित आउटपुट की अनुमति देता है। यहाँ एक सरल उदाहरण है:

```c
#include <stdio.h>

int main() {
    int x = 5;
    printf("Debug: x का मान %d है\n", x);
    
    // आपका प्रोग्राम तर्क यहाँ
    
    return 0;
}
```

नमूना आउटपुट:

```
Debug: x का मान 5 है
```

अधिक सोफिस्टिकेटेड डिबग प्रिंटिंग के लिए, आप फाइल नाम और लाइन नंबर की जानकारी शामिल करना चाहेंगे। इसे `__FILE__` और `__LINE__` पूर्वनिर्धारित मैक्रो का उपयोग करके किया जा सकता है जैसे कि:

```c
#define DEBUG_PRINT(fmt, args...) fprintf(stderr, "DEBUG: %s:%d: " fmt, __FILE__, __LINE__, ##args)

int main() {
    int testValue = 10;
    DEBUG_PRINT("परीक्षण मूल्य %d है\n", testValue);
    
    // आपका प्रोग्राम तर्क यहाँ
    
    return 0;
}
```

नमूना आउटपुट:

```
DEBUG: example.c:6: परीक्षण मूल्य 10 है
```

ध्यान दें कि इस उदाहरण में, हम `stderr` पर आउटपुट करने के लिए `fprintf` का उपयोग कर रहे हैं, जो अक्सर डिबग संदेशों के लिए अधिक उपयुक्त होता है।

## गहराई में जानकारी
ऐतिहासिक रूप से, C में डिबगिंग तकनीक मैनुअल और प्रारंभिक रही हैं, भाषा के मूल दर्शन और उम्र के कारण। जहां आधुनिक भाषाएँ सोफिस्टिकेटेड, बिल्ट-इन डिबगिंग लाइब्रेरीज़ शामिल कर सकती हैं या इंटीग्रेटेड डेवलपमेंट एनवायरनमेंट (IDE) फीचर्स पर भारी निर्भर कर सकती हैं, C प्रोग्रामर अक्सर अपने प्रोग्राम के निष्पादन को ट्रेस करने के लिए ऊपर दिखाए गए प्रिंट स्टेटमेंट्स को मैन्युअली डालकर रिसॉर्ट करते हैं।

डिबग प्रिंट्स के साथ एक सावधानी उनके आउटपुट को गंदा करने की संभावना और उत्पादन कोड में गलती से छोड़ने पर प्रदर्शन मुद्दों के लिए है। इन कारणों से, कंडीशनल कम्पाइलेशन (जैसे, `#ifdef DEBUG ... #endif`) का उपयोग करना एक बेहतर दृष्टिकोण हो सकता है, जो डिबग स्टेटमेंट्स को संकलन-समय फ्लैग के आधार पर शामिल या बाहर करने की अनुमति देता है।

इसके अलावा, C डिबगिंग के लिए अब अधिक उन्नत उपकरण और लाइब्रेरीज़ उपलब्ध हैं, जैसे कि GDB (GNU Debugger) और Valgrind मेमोरी लीक डिटेक्शन के लिए। ये उपकरण एक अधिक एकीकृत डिबगिंग दृष्टिकोण प्रदान करते हैं, बिना प्रिंट स्टेटमेंट्स डालकर कोड में संशोधन करने की आवश्यकता के।

फिर भी, `printf` डिबगिंग की सादगी और तत्काल प्रतिक्रिया को कम नहीं आंका जा सकता, इसे प्रोग्रामर के टूलबॉक्स में एक उपयोगी उपकरण बनाता है, विशेषकर उनके लिए जो C की जटिलताओं को सीख रहे हैं।
