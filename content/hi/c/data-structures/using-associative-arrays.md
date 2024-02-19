---
aliases:
- /hi/c/using-associative-arrays/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:58.738135-07:00
description: "`Associative arrays`, \u091C\u093F\u0928\u094D\u0939\u0947\u0902 \u0926\
  \u0942\u0938\u0930\u0940 \u092D\u093E\u0937\u093E\u0913\u0902 \u092E\u0947\u0902\
  \ \u092E\u0948\u092A\u094D\u0938 \u092F\u093E \u0921\u093F\u0915\u094D\u0936\u0928\
  \u0930\u0940\u091C \u0915\u0947 \u0930\u0942\u092A \u092E\u0947\u0902 \u091C\u093E\
  \u0928\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u092E\u0939\u0924\u094D\u0935\
  \u092A\u0942\u0930\u094D\u0923-\u092E\u0942\u0932\u094D\u092F \u0915\u0947 \u091C\
  \u094B\u0921\u093C\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u093F\
  \u0928\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0921\u0947\u091F\u093E \u0916\
  \u094B\u091C \u0914\u0930\u2026"
lastmod: 2024-02-18 23:09:04.196523
model: gpt-4-0125-preview
summary: "`Associative arrays`, \u091C\u093F\u0928\u094D\u0939\u0947\u0902 \u0926\u0942\
  \u0938\u0930\u0940 \u092D\u093E\u0937\u093E\u0913\u0902 \u092E\u0947\u0902 \u092E\
  \u0948\u092A\u094D\u0938 \u092F\u093E \u0921\u093F\u0915\u094D\u0936\u0928\u0930\
  \u0940\u091C \u0915\u0947 \u0930\u0942\u092A \u092E\u0947\u0902 \u091C\u093E\u0928\
  \u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u092E\u0939\u0924\u094D\u0935\u092A\
  \u0942\u0930\u094D\u0923-\u092E\u0942\u0932\u094D\u092F \u0915\u0947 \u091C\u094B\
  \u0921\u093C\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902 \u091C\u093F\u0928\
  \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0921\u0947\u091F\u093E \u0916\u094B\
  \u091C \u0914\u0930\u2026"
title: "\u0938\u0902\u0918\u091F\u0915 \u0905\u0930\u0947\u091C\u093C \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917"
---

{{< edit_this_page >}}

## क्या और क्यों?

`Associative arrays`, जिन्हें दूसरी भाषाओं में मैप्स या डिक्शनरीज के रूप में जाना जाता है, महत्वपूर्ण-मूल्य के जोड़े होते हैं जिनका उपयोग डेटा खोज और संपादन में कुशलता से किया जाता है। पारंपरिक एरेज़ के विपरीत जो इंटीजर इंडेक्सेस का उपयोग करते हैं, associative arrays कुंजीयों का उपयोग करते हैं, जो प्रोग्रामरों के लिए डेटा एक्सेस को अधिक सहज और लचीला बनाते हैं।

## कैसे करें:

C में कुछ उच्च स्तरीय भाषाओं की तरह Associative arrays के लिए बिल्ट-इन समर्थन नहीं है, लेकिन आप इसे संरचनाओं और हैशिंग का उपयोग करके सिमुलेट कर सकते हैं। नीचे एक सरल उदाहरण है जो एक संरचना और एक सरल हैशिंग फ़ंक्शन के संयोजन का उपयोग करके स्ट्रिंग कुंजियों द्वारा पूर्णांकों को संग्रहीत और एक्सेस करने के लिए एक associative array को लागू करने का प्रदर्शित करता है।

पहले, एक-एक कुंजी-मूल्य जोड़े को प्रस्तुत करने के लिए एक संरचना को परिभाषित करें और दूसरा associative array स्वयं को प्रस्तुत करने के लिए:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // आउटपुट: 1
    printf("%d\n", find(&a, "key2")); // आउटपुट: 2

    return 0;
}
```

यह उदाहरण बुनियादी कार्यों का प्रदर्शन करता है: एक associative array को आरंभ करना, महत्वपूर्ण-मूल्य जोड़े डालना, और कुंजियों द्वारा मूल्यों को खोजना। ध्यान दें कि इस कोड में टक्कर को संभालने की कमी है और यह शैक्षिक उद्देश्यों के लिए है।

## गहराई से अध्ययन

Associative arrays की अवधारणा C से पहले की है, लेकिन भाषा का निम्न-स्तरीय स्वभाव सीधे तौर पर उन्हें बिल्ट-इन प्रकारों के रूप में समर्थन नहीं करता है। यह डेटा संरचनाओं और एल्गोरिदमों, सहित हैशिंग तंत्रों की, जो कुशल महत्वपूर्ण-मूल्य मैपिंग के लिए हैं, की गहरी समझ को प्रोत्साहित करता है। कई C पुस्तकालयों और ढांचे, जैसे कि GLib के `GHashTable`, पूर्ण टक्कर हैंडलिंग, गतिशील आकार बदलना, और विभिन्न प्रकार की कुंजी और मूल्य प्रकारों के लिए समर्थन के साथ एक मजबूत कार्यान्वयन प्रदान करते हैं।

C में associative arrays का मैनुअल निर्माण बिल्ट-इन समर्थन वाली भाषाओं की तुलना में जटिल माना जा सकता है, लेकिन यह डेटा संरचनाओं की आंतरिक कार्यप्रणाली के बारे में अमूल्य अंतर्दृष्टि प्रदान करता है, समस्या-समाधान और अनुकूलन में एक प्रोग्रामर के कौशल को तेज करता है। हालांकि, उत्पादन कोड या अधिक जटिल अनुप्रयोगों के लिए, मौजूदा पुस्तकालयों जैसे कि GLib का लाभ उठाना अक्सर एक अधिक व्यावहारिक और समय-कुशल दृष्टिकोण होता है।
