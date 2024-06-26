---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:51.328863-07:00
description: "\u0915\u0948\u0938\u0947: Arduino \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u090F\u0915 SD \u0915\u093E\u0930\
  \u094D\u0921 \u092A\u0930 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\
  \u0907\u0932 \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F, \u0906\u092A\u0915\u094B \u092A\u0939\u0932\u0947 `SD.h` \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u094B \u0936\u093E\u092E\u093F\
  \u0932 \u0915\u0930\u0928\u093E \u0939\u094B\u0917\u093E, \u091C\u094B SD \u0915\
  \u093E\u0930\u094D\u0921\u094D\u0938 \u0915\u0947 \u0938\u093E\u0925 \u0907\u0902\
  \u091F\u0930\u0947\u0915\u094D\u091F\u2026"
lastmod: '2024-03-13T22:44:52.805686-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947\
  \ \u0939\u0941\u090F \u090F\u0915 SD \u0915\u093E\u0930\u094D\u0921 \u092A\u0930\
  \ \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932 \u092E\u0947\
  \u0902 \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A\
  \u0915\u094B \u092A\u0939\u0932\u0947 `SD.h` \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0928\
  \u093E \u0939\u094B\u0917\u093E, \u091C\u094B SD \u0915\u093E\u0930\u094D\u0921\u094D\
  \u0938 \u0915\u0947 \u0938\u093E\u0925 \u0907\u0902\u091F\u0930\u0947\u0915\u094D\
  \u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0906\u0935\u0936\
  \u094D\u092F\u0915 \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u0940 \u0939\u0948\u0964 \u0938\u0941\
  \u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\u0947\u0902 \u0915\u093F\
  \ \u0906\u092A\u0915\u093E Arduino \u092C\u094B\u0930\u094D\u0921 \u090F\u0915 SD\
  \ \u0915\u093E\u0930\u094D\u0921 \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0938\
  \u0947 \u091C\u0941\u0921\u093C\u093E \u0939\u0941\u0906 \u0939\u0948\u0964."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

## कैसे:
Arduino का उपयोग करते हुए एक SD कार्ड पर टेक्स्ट फाइल में लिखने के लिए, आपको पहले `SD.h` लाइब्रेरी को शामिल करना होगा, जो SD कार्ड्स के साथ इंटरेक्ट करने के लिए आवश्यक फंक्शन्स प्रदान करती है। सुनिश्चित करें कि आपका Arduino बोर्ड एक SD कार्ड मॉड्यूल से जुड़ा हुआ है।

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // 9600 बिट्स प्रति सेकंड पर सीरियल कम्युनिकेशन आरंभ करें:
  Serial.begin(9600);
  
  // SD कार्ड इनिशियलाइजेशन की जाँच करें
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // फाइल खोलें। ध्यान दें कि एक समय में केवल एक फाइल ही खुली रह सकती है,
  // इसलिए एक और खोलने से पहले आपको इसे बंद करना होगा।
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // यदि फाइल ठीक से खुली हो, तो इसमें लिखें:
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing text file write.");
    // फाइल को बंद करें:
    myFile.close();
    Serial.println("done.");
  } else {
    // यदि फाइल नहीं खुली, तो एक त्रुटि प्रिंट करें:
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // सेटअप के बाद कुछ नहीं होता
}
```

### नमूना आउटपुट:
जब आप यह कोड चलाते हैं, तो Arduino IDE सीरियल मॉनिटर दिखाएगा:
```
Initialization done.
Writing to test.txt...done.
```
यदि डेटा सही ढंग से लिखा गया था, यह जाँचने के लिए, आप Arduino से SD कार्ड को हटा सकते हैं, इसे एक कंप्यूटर में डाल सकते हैं, और `test.txt` फाइल को खोलकर संदेश "Testing text file write." देख सकते हैं।

अधिक उन्नत फ़ाइल ऑपरेशन्स या प्रोसेसिंग की आवश्यकता वाली परियोजनाओं के लिए, अधिक लाइब्रेरीज का पता लगाने या आपकी विशिष्ट आवश्यकताओं के अनुकूलित कस्टम फंक्शन्स लिखने पर विचार करें।
