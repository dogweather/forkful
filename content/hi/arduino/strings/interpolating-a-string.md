---
date: 2024-01-20 17:50:13.812227-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Arduino\
  \ \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 interpolation\
  \ \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0932\u0940 \u0938\u0902\u092D\
  \u0935 \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u093E \u091C\u0948\u0938\u0947\
  \ \u0915\u0941\u091B \u0926\u0942\u0938\u0930\u0947 \u0939\u093E\u0908-\u0932\u0947\
  \u0935\u0932 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917\
  \ \u0932\u0948\u0902\u0917\u094D\u0935\u0947\u091C\u0947\u091C \u092E\u0947\u0902\
  \ \u0939\u094B\u0924\u093E \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0939\u092E\
  \u2026"
lastmod: '2024-03-13T22:44:52.750590-06:00'
model: gpt-4-1106-preview
summary: "Arduino \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ interpolation \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0932\u0940 \u0938\
  \u0902\u092D\u0935 \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u093E \u091C\u0948\
  \u0938\u0947 \u0915\u0941\u091B \u0926\u0942\u0938\u0930\u0947 \u0939\u093E\u0908\
  -\u0932\u0947\u0935\u0932 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u093F\u0902\u0917 \u0932\u0948\u0902\u0917\u094D\u0935\u0947\u091C\u0947\u091C\
  \ \u092E\u0947\u0902 \u0939\u094B\u0924\u093E \u0939\u0948, \u0932\u0947\u0915\u093F\
  \u0928 \u0939\u092E `String` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\
  \u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0938\
  \u093F\u092E\u093F\u0932\u0930 \u0906\u0909\u091F\u0915\u092E \u0939\u093E\u0938\
  \u093F\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\
  \u0939\u093E\u0902 \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948\
  ."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
Arduino में स्ट्रिंग interpolation डायरेक्टली संभव नहीं होता जैसे कुछ दूसरे हाई-लेवल प्रोग्रामिंग लैंग्वेजेज में होता है, लेकिन हम `String` ऑब्जेक्ट्स का उपयोग करके सिमिलर आउटकम हासिल कर सकते हैं। यहां एक उदाहरण है:

```arduino
String name = "राहुल";
int age = 30;

String message = "नाम: " + name + ", उम्र: " + String(age);

Serial.begin(9600);
Serial.println(message);
```

सैम्पल आउटपुट:
```
नाम: राहुल, उम्र: 30
```

## Deep Dive (गहराई से जानकारी):
स्ट्रिंग interpolation का कॉन्सेप्ट ओल्ड स्कूल प्रोग्रामिंग जैसे C के `sprintf` फंक्शन से लेकर, रूबी, पायथन, और जावास्क्रिप्ट जैसी नई लैंग्वेजेज के Template literals तक विस्तृत है। Arduino प्लेटफॉर्म पर हम PROGMEM का उपयोग करके मेमोरी की बचत कर सकते हैं, और `sprintf` या `snprintf` का उपयोग करके बिना मेमोरी के अधिक उपयोग के अधिक कॉम्पलेक्स स्ट्रिंग्स को फॉर्मेट कर सकते हैं। जबकि `String` ऑब्जेक्ट्स सुविधाजनक होते हैं, वे डायनामिक मेमोरी एलोकेशन का उपयोग करते हैं जो कि मेमोरी फ्रैग्मेन्टेशन का कारण बन सकता है।

## See Also (यह भी देखें):
- Arduino `String` class reference: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- `snprintf()` function: [Cplusplus.com Reference](http://www.cplusplus.com/reference/cstdio/snprintf/)
- Memory management with PROGMEM: [Arduino Memory](https://www.arduino.cc/reference/en/language/variables/utilities/progmem/)
