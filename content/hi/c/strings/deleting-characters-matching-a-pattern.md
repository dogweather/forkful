---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:46.729627-07:00
description: "C \u092E\u0947\u0902 \u090F\u0915 \u0935\u093F\u0936\u093F\u0937\u094D\
  \u091F \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u093F\u0932\u093E\
  \u0928 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\u0915\u094D\u0937\
  \u0930\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0938\u0947 \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u0909\u0928\
  \ \u0938\u092D\u0940 \u0909\u0926\u093E\u0939\u0930\u0923\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u0947 \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\
  \u0902 \u0939\u0948 \u091C\u094B \u092A\u0942\u0930\u094D\u0935\u0928\u093F\u0930\
  \u094D\u0927\u093E\u0930\u093F\u0924 \u092E\u093E\u092A\u0926\u0902\u0921\u094B\u0902\
  \ \u0915\u094B \u0938\u0902\u0924\u0941\u0937\u094D\u091F\u2026"
lastmod: '2024-03-13T22:44:53.112109-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902 \u090F\u0915 \u0935\u093F\u0936\u093F\u0937\u094D\u091F\
  \ \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u093F\u0932\u093E\u0928\
  \ \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\u0915\u094D\u0937\u0930\
  \u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u0909\u0928 \u0938\
  \u092D\u0940 \u0909\u0926\u093E\u0939\u0930\u0923\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u0947 \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902\
  \ \u0939\u0948 \u091C\u094B \u092A\u0942\u0930\u094D\u0935\u0928\u093F\u0930\u094D\
  \u0927\u093E\u0930\u093F\u0924 \u092E\u093E\u092A\u0926\u0902\u0921\u094B\u0902\
  \ \u0915\u094B \u0938\u0902\u0924\u0941\u0937\u094D\u091F\u2026"
title: "\u090F\u0915 \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\
  \u0932 \u0916\u093E\u0924\u0947 \u0935\u0930\u094D\u0923\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

C में एक विशिष्ट पैटर्न से मिलान करने वाले अक्षरों को स्ट्रिंग से डिलीट करना उन सभी उदाहरणों को हटाने के बारे में है जो पूर्वनिर्धारित मापदंडों को संतुष्ट करते हैं। प्रोग्रामर इस कार्य को इनपुट्स को सेनिटाइज़ करने, डेटा को प्रोसेसिंग के लिए तैयार करने, या बस स्ट्रिंग्स को आउटपुट या आगे के मैनिप्यूलेशन के लिए साफ करने के लिए करते हैं, सुनिश्चित करते हैं कि संभाला जा रहा डेटा दी गई संदर्भ या एल्गोरिथ्म के लिए बिल्कुल आवश्यक हो।

## कैसे करें:

C में एक पैटर्न के आधार पर स्ट्रिंग से अक्षरों को प्रत्यक्ष रूप से डिलीट करने के लिए कोई बिल्ट-इन फंक्शन नहीं आता है, कुछ उच्च स्तरीय भाषाओं के विपरीत। हालांकि, आप स्ट्रिंग पर मैन्युअल रूप से इटरेटिंग करके और एक नई स्ट्रिंग का निर्माण करके जो अवांछित अक्षरों को छोड़ देती है, आसानी से यह कार्य पूरा कर सकते हैं। उदाहरण के लिए, मान लीजिए आप एक स्ट्रिंग से सभी अंकों को हटाना चाहते हैं। आप इसे निम्नलिखित तरीके से कर सकते हैं:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C Programming 101: The Basics!";
    remove_digits(str);
    printf("Result: %s\n", str);
    return 0;
}
```

नमूना आउटपुट:
```
Result: C Programming : The Basics!
```

यह उदाहरण `ctype.h` से `isdigit` का उपयोग करके अंकों की पहचान करता है, गैर-अंक अक्षरों को स्ट्रिंग की शुरुआत में शिफ्ट करता है और सभी अक्षरों के मूल्यांकन के बाद स्ट्रिंग को समाप्त करता है।

## गहराई से

प्रस्तुत समाधान उन अवांछित अक्षरों को प्रभावी रूप से फिल्टर करने के लिए एक ही ऐरे के भीतर एक दो-पॉइंटर दृष्टिकोण का उपयोग करता है, एक तकनीक जो C के हाथों-पर मेमोरी मैनेजमेंट दर्शन का प्रतीक है। यह विधि कारगर है क्योंकि यह जगह में ही काम करती है, अतिरिक्त मेमोरी आबंटन की आवश्यकता से बचती है और इस प्रकार ओवरहेड को कम से कम करती है।

ऐतिहासिक रूप से, C में उच्च-स्तरीय स्ट्रिंग मैनिप्यूलेशन फंक्शन्स की अनुपस्थिति ने प्रोग्रामरों को स्मृति स्तर पर स्ट्रिंग हैंडलिंग की गहरी समझ विकसित करने पर मजबूर किया है, ऊपर दिए गए जैसे नवीन दृष्टिकोणों की ओर ले जाता है। जबकि इससे अधिक नियंत्रण और कार्यकुशलता का लाभ होता है, इसके साथ बफर ओवरफ्लो और एक-से-अधिक गलतियों जैसी गलतियों का उच्च जोखिम भी आता है।

आधुनिक विकास संदर्भों में, विशेष रूप से सुरक्षा और सुरक्षा पर जोर देने वाले, ऐसे कम-स्तरीय कार्यों को दूर करने वाली भाषाओं को स्ट्रिंग मैनिप्यूलेशन कार्यों के लिए प्राथमिकता दी जा सकती है। फिर भी, इन C तकनीकों को समझना और उपयोग करना उन परिदृश्यों के लिए अमूल्य रहता है जो महीन दक्षता अनुकूलन या उन वातावरणों की मांग करते हैं जहां C की न्यूनतावाद और गति सर्वोपरि हैं।
