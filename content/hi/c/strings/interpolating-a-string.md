---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:48.975190-07:00
description: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\
  \u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u0932\u093F\u091F\u0930\u0932\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u092D\
  \u0940\u0924\u0930 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\
  \u094B\u0902 \u0915\u094B \u0938\u092E\u093E\u0939\u093F\u0924 \u0915\u0930\u0915\
  \u0947 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E\
  \ \u0928\u093F\u0930\u094D\u092E\u093E\u0923 \u0915\u0930\u0928\u0947 \u0915\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \u2026"
lastmod: '2024-03-13T22:44:53.115887-06:00'
model: gpt-4-0125-preview
summary: "\u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u092E\u0947\u0902, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\
  \u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928 \u0932\u093F\u091F\u0930\u0932\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u092D\
  \u0940\u0924\u0930 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\
  \u094B\u0902 \u0915\u094B \u0938\u092E\u093E\u0939\u093F\u0924 \u0915\u0930\u0915\
  \u0947 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u093E\
  \ \u0928\u093F\u0930\u094D\u092E\u093E\u0923 \u0915\u0930\u0928\u0947 \u0915\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u093E \u0907\u0902\
  \u091F\u0930\u092A\u094B\u0932\u0947\u0936\u0928"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामिंग में, स्ट्रिंग इंटरपोलेशन लिटरल स्ट्रिंग्स के भीतर अभिव्यक्तियों को समाहित करके स्ट्रिंग्स का निर्माण करने की प्रक्रिया है। प्रोग्रामर्स इसे जानकारीपूर्ण संदेश, गतिशील पूछताछ, या चर सामग्री वाली किसी भी स्ट्रिंग को कुशलता और साफ़-सफ़ाई से निर्मित करने के लिए करते हैं, अक्सर उपयोगकर्ता आउटपुट या लॉगिंग के उद्देश्यों के लिए।

## कैसे:

C, कुछ उच्च-स्तरीय भाषाओं के विपरीत, अपने सिंटैक्स में सीधे स्ट्रिंग इंटरपोलेशन का समर्थन नहीं करता। इसके बजाय, चर सामग्री वाले स्ट्रिंग निर्माण को आमतौर पर `printf` फंक्शन या इसके संस्करणों का उपयोग करके आउटपुट के लिए, और `sprintf` का उपयोग करके स्ट्रिंग निर्माण के लिए हासिल किया जाता है। यहाँ C में गतिशील रूप से स्ट्रिंग्स का निर्माण कैसे करें, पर एक नज़र है:

```c
#include <stdio.h>

int main() {
    char name[] = "Jane Doe";
    int age = 28;

    // आउटपुट के लिए printf का उपयोग करना
    printf("Hello, my name is %s and I am %d years old.\n", name, age);

    // स्ट्रिंग निर्माण के लिए sprintf का उपयोग करना
    char info[50];
    sprintf(info, "Name: %s, Age: %d", name, age);
    printf("%s\n", info);

    return 0;
}
```
नमूना आउटपुट:
```
Hello, my name is Jane Doe and I am 28 years old.
Name: Jane Doe, Age: 28
```
ये स्निपेट C में चर डेटा को स्ट्रिंग्स में शामिल करने के पारंपरिक तरीके का प्रदर्शन करते हैं, विस्तृत स्ट्रिंग्स का निर्माण करने में लचीलापन प्रदान करते हैं।

## गहराई से जानकारी

बिल्ट-इन स्ट्रिंग इंटरपोलेशन सुविधाओं वाली अधिक आधुनिक प्रोग्रामिंग भाषाओं के आगमन से पहले, C डेवलपर्स को चर सामग्री वाले स्ट्रिंग्स को रचने के लिए `sprintf()`, `snprintf()`, और उनके संस्करणों जैसे फंक्शनों पर निर्भर रहना पड़ा था। यह दृष्टिकोण, जबकि प्रभावी, `sprintf()` के साथ विशेष रूप से सावधानी से प्रबंधित नहीं करने पर बफर ओवरफ्लो जैसे संभावित जोखिमों को पेश करता है।

विकल्पों पर विचार करते हुए, पायथन और जावास्क्रिप्ट जैसी भाषाओं ने स्ट्रिंग लिटरल्स के भीतर सीधे अभिव्यक्तियों को एम्बेड करने की अनुमति देने वाली अधिक सहज स्ट्रिंग इंटरपोलेशन सुविधाओं को पेश किया, जैसे कि एफ-स्ट्रिंग्स (फॉर्मेटेड स्ट्रिंग लिटरल्स) और टेम्पलेट लिटरल्स क्रमशः। ये सुविधाएँ डेवलपर्स को कोड को और अधिक पठनीय और संक्षिप्त बनाने की अनुमति देती हैं।

C के संदर्भ में, बिल्ट-इन स्ट्रिंग इंटरपोलेशन सुविधाओं की अनुपस्थिति के बावजूद, इसका दृष्टिकोण, जो लोग सटीक प्रारूपण नियंत्रण की आवश्यकता रखते हैं, उनके लिए एक लाभ के रूप में और नवागंतुकों या उन लोगों के लिए जटिलता के रूप में देखा जा सकता है जो तेज़ी से, अधिक पठनीय समाधानों की खोज कर रहे हैं। C99 में `snprintf()` का परिचय सुरक्षा संबंधी चिंताओं को कम करते हुए डेवलपर्स को लिखे जाने वाले अधिकतम बाइट्स की संख्या निर्दिष्ट करने की अनुमति देता है, जिससे स्ट्रिंग प्रारूपण सुरक्षित हो जाता है।

जबकि C की विधि आधुनिक भाषाओं की तुलना में वर्बोस या जटिल प्रतीत हो सकती है, इसकी स्ट्रिंग हैंडलिंग मैकेनिज़्म की समझ सॉफ्टवेयर विकास में अधिक अमूर्त अवधारणाओं को समझने के लिए एक मजबूत नींव प्रदान करती है, जोर देती है कि निचले स्तर पर मेमोरी प्रबंधन और डेटा प्रारूपण का महत्व है।
