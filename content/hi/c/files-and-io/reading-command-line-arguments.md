---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:24.239339-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u0940 \u092E\
  \u0947\u0902, `main` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u094B \u0915\u092E\
  \u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\u0917\u094D\u092F\
  \u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u0938\u094D\u0935\u0940\u0915\u093E\u0930\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `int argc` \u0914\u0930\
  \ `char *argv[]` \u092A\u0948\u0930\u093E\u092E\u0940\u091F\u0930\u094D\u0938 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0921\u093F\u091C\
  \u093C\u093E\u0907\u0928 \u0915\u093F\u092F\u093E \u091C\u093E\u2026"
lastmod: '2024-03-13T22:44:53.172524-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u0940 \u092E\u0947\u0902, `main` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0915\u094B \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\
  \u094D\u0917\u094D\u092F\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u0938\u094D\u0935\
  \u0940\u0915\u093E\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `int argc` \u0914\u0930 `char *argv[]` \u092A\u0948\u0930\u093E\u092E\u0940\u091F\
  \u0930\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0921\u093F\u091C\u093C\u093E\u0907\u0928 \u0915\u093F\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0902, `argc`\
  \ \u092A\u093E\u0938 \u0915\u093F\u090F \u0917\u090F \u0906\u0930\u094D\u0917\u094D\
  \u092F\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u0940 \u0938\u0902\u0916\
  \u094D\u092F\u093E \u0915\u094B \u0926\u0930\u094D\u0936\u093E\u0924\u093E \u0939\
  \u0948, \u0914\u0930 `argv` \u0938\u092D\u0940 \u0906\u0930\u094D\u0917\u094D\u092F\
  \u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u094B \u0938\u0942\u091A\u0940\
  \u092C\u0926\u094D\u0927 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\
  \u0948\u0930\u0947\u0915\u094D\u091F\u0930 \u092A\u0949\u0907\u0902\u091F\u0930\u094D\
  \u0938 \u0915\u093E \u090F\u0915 \u0910\u0930\u0947 \u0939\u0948\u0964 \u0928\u093F\
  \u092E\u094D\u0928\u0932\u093F\u0916\u093F\u0924 \u090F\u0915 \u0924\u094D\u0935\
  \u0930\u093F\u0924 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0924\u0930\u094D\
  \u0915\u094B\u0902 \u0915\u094B \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## कैसे करें:
सी में, `main` फंक्शन को कमांड लाइन आर्ग्युमेंट्स स्वीकार करने के लिए `int argc` और `char *argv[]` पैरामीटर्स का उपयोग करके डिज़ाइन किया जा सकता है। यहां, `argc` पास किए गए आर्ग्युमेंट्स की संख्या को दर्शाता है, और `argv` सभी आर्ग्युमेंट्स को सूचीबद्ध करने वाले कैरेक्टर पॉइंटर्स का एक ऐरे है। निम्नलिखित एक त्वरित उदाहरण है:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("प्रोग्राम नाम: %s\n", argv[0]);
    printf("आर्ग्युमेंट्स की संख्या: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("आर्ग्युमेंट %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

उपर्युक्त कोड का उपयोग करके, यदि प्रोग्राम को `./programName -a example` के रूप में निष्पादित किया जाता है, तो आउटपुट होगा:

```
प्रोग्राम नाम: ./programName
आर्ग्युमेंट्स की संख्या: 2
आर्ग्युमेंट 1: -a
आर्ग्युमेंट 2: example
```

यह दिखाता है कि कमांड लाइन आर्ग्युमेंट्स को एक सी प्रोग्राम में कैसे पार्स और उपयोग किया जा सकता है।

## गहन अध्ययन
कार्यक्रमों को आर्ग्युमेंट्स पास करने की प्रथा यूनिक्स के प्रारंभिक दिनों से चली आ रही है। इस पारंपरिक दृष्टिकोण में, `argc` और `argv` कमांड लाइन इंटरैक्शन के लिए एक सरल फिर भी शक्तिशाली इंटरैक्शन प्रदान करते हैं, जो यूनिक्स के छोटे, मॉड्यूलर उपकरणों के दर्शन को अवतारित करते हैं जो एक साथ काम करते हैं। जबकि आधुनिक भाषाएँ अक्सर कमांड-लाइन आर्ग्युमेंट्स को पार्स करने के लिए अधिक सोफ़िस्टिकेटेड लाइब्रेरीज़ या फ्रेमवर्क पेश करती हैं, सी की विधि की सीधापन अप्रतिम पारदर्शिता और नियंत्रण प्रदान करती है।

हाल के विकासों में, `getopt` जैसी लाइब्रेरीज़ POSIX सिस्टम्स में लंबे विकल्प नामों को संभालने या लापता आर्ग्युमेंट्स के लिए डिफॉल्ट मानों का समर्थन करने की तरह पेचीदा पार्सिंग आवश्यकताओं को समर्थन करने के लिए विकसित हुई हैं। फिर भी, `argc` और `argv` का मूल तंत्र सी में कार्यक्रमों के अपने रन-टाइम वातावरण के साथ इंटरेक्शन को समझने के लिए अत्यावश्यक बना हुआ है।

आलोचक `argc` और `argv` के साथ सीधे डील करने को त्रुटि-प्रवण मान सकते हैं, उच्च-स्तरीय अमूर्तताओं का उपयोग करने का आग्रह करते हैं। फिर भी, सी की बारीकियों को महारत से सीखने और इसके निम्न-स्तरीय संचालन की बारीकियों को सराहना करने वालों के लिए, कमांड लाइन आर्ग्युमेंट पार्सिंग को महारत हासिल करना एक रस्म है। ऐतिहासिक पद्धति और व्यावहारिक उपयोगिता का यह मेल सिस्टम प्रोग्रामिंग और सॉफ्टवेयर विकास में सी की स्थायी अपील को संक्षेप में प्रस्तुत करता है।
