---
date: 2024-01-20 17:50:13.812227-07:00
description: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\
  \u0948 \u091C\u093F\u0938\u092E\u0947\u0902 \u0939\u092E \u0935\u0947\u0930\u093F\
  \u090F\u092C\u0932\u094D\u0938 \u0914\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\
  \u0930\u0947\u0936\u0902\u0938 \u0915\u094B \u0921\u093E\u092F\u0930\u0947\u0915\
  \u094D\u091F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902\
  \ \u0907\u0902\u091C\u0947\u0915\u094D\u091F \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902\u0964 \u0907\u0938\u0938\u0947 \u0915\u094B\u0921 \u0938\u093E\u092B \u0914\
  \u0930 \u092A\u0922\u093C\u0928\u0947 \u092E\u0947\u0902 \u0906\u0938\u093E\u0928\
  \ \u0939\u094B\u0924\u093E \u0939\u0948, \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:52.750590-06:00'
model: gpt-4-1106-preview
summary: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\
  \ \u091C\u093F\u0938\u092E\u0947\u0902 \u0939\u092E \u0935\u0947\u0930\u093F\u090F\
  \u092C\u0932\u094D\u0938 \u0914\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0902\u0938 \u0915\u094B \u0921\u093E\u092F\u0930\u0947\u0915\u094D\
  \u091F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0907\
  \u0902\u091C\u0947\u0915\u094D\u091F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0907\u0938\u0938\u0947 \u0915\u094B\u0921 \u0938\u093E\u092B \u0914\u0930\
  \ \u092A\u0922\u093C\u0928\u0947 \u092E\u0947\u0902 \u0906\u0938\u093E\u0928 \u0939\
  \u094B\u0924\u093E \u0939\u0948, \u0914\u0930\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## What & Why? (क्या और क्यों?)
String interpolation एक तरीका है जिसमें हम वेरिएबल्स और एक्सप्रेशंस को डायरेक्ट स्ट्रिंग में इंजेक्ट करते हैं। इससे कोड साफ और पढ़ने में आसान होता है, और डायनामिकली टेक्स्ट मैसेजेज बनाने में मदद मिलती है।

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
