---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- /hi/arduino/writing-a-text-file.md
date:                  2024-02-03T19:27:51.328863-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Arduino में एक टेक्स्ट फाइल लिखना इसका अर्थ है एक SD कार्ड या इसी तरह के संग्रहण मॉड्यूल पर डेटा को फाइल में सहेजना, जो अक्सर डेटा लॉगिंग उद्देश्यों के लिए होता है। प्रोग्रामर इसे सेंसर रीडिंग्स रिकॉर्ड करने, कॉन्फिगरेशन सहेजने, या समय के साथ एप्लिकेशन इवेंट्स लॉग करने के लिए करते हैं, जो डेटा विश्लेषण या ट्रैकिंग की आवश्यकता वाली परियोजनाओं के लिए अत्यंत महत्वपूर्ण होता है।

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
