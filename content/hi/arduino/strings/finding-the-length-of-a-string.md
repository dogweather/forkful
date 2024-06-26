---
date: 2024-01-20 17:48:04.161359-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Arduino \u0915\u094B\
  \u0921 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\
  \u0940 \u0932\u092E\u094D\u092C\u093E\u0908 \u091C\u093E\u0928\u0928\u093E \u0938\
  \u0930\u0932 \u0939\u0948. `length()` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0906\u092A \u0910\
  \u0938\u093E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.759026-06:00'
model: gpt-4-1106-preview
summary: "Arduino \u0915\u094B\u0921 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0915\u0940 \u0932\u092E\u094D\u092C\u093E\u0908 \u091C\u093E\
  \u0928\u0928\u093E \u0938\u0930\u0932 \u0939\u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

## कैसे करें:
Arduino कोड में स्ट्रिंग की लम्बाई जानना सरल है. `length()` फंक्शन का उपयोग करके आप ऐसा कर सकते हैं:

```arduino
void setup() {
  // सीरियल कम्यूनिकेशन शुरू करें
  Serial.begin(9600);
  
  // स्ट्रिंग बनाएं
  String myString = "ArduinoProgramming";
  
  // लम्बाई निकालें और प्रिंट करें
  unsigned int lengthOfString = myString.length();
  Serial.print("String Length: ");
  Serial.println(lengthOfString);
}

void loop() {
  // कुछ नहीं करना है यहाँ
}
```

सैंपल आउटपुट होगा:
```
String Length: 18
```

## गहराई में:
स्ट्रिंग की लम्बाई पता करने का तरीका अरुइनो प्रोग्रामिंग का एक बुनियादी हिस्सा है। पहले के समय में, C जैसी प्रोग्रामिंग लैंग्वेज में, इसे पता करने के लिए `strlen()` फंक्शन का इस्तेमाल होता था, जो कि नल-टर्मिनेटेड स्ट्रिंग्स के लिए काम करता है। Arduino में `length()` फंक्शन उससे ज्यादा सीधा और सुविधाजनक है।

यह भी जरूरी है कि हम जब लम्बाई नापें तो परफॉर्मेंस का भी ख्याल रखें क्योंकि हर बार `length()` कॉल करना CPU टाइम बर्बाद कर सकता है अगर लम्बाई बार-बार नहीं बदल रही हो तो।

## और देखें:
स्ट्रिंग्स और उनके ऑपरेशन्स को बेहतर समझने के लिए निम्न लिंक्स की जांच करें:

- Arduino Official String Reference: [Arduino - StringObject](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- C++ `std::string` Reference: [std::string - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string)
