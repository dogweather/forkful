---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:56.783439-07:00
description: "Arduino \u092E\u0947\u0902 CSV (Comma-Separated Values) \u092B\u093C\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0930\u0928\u093E \u090F\u0938\u0921\u0940 \u0915\u093E\u0930\u094D\u0921\
  \ \u092A\u0930 \u0938\u0902\u0917\u094D\u0930\u0939\u0940\u0924 \u0906\u092E\u0924\
  \u094C\u0930 \u092A\u0930 CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0938\u0947\
  \ \u092A\u0922\u093C\u0928\u0947 \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\
  \u093F\u0916\u0928\u0947 \u0915\u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\
  \u0924\u093E \u0939\u0948, \u091C\u094B\u2026"
lastmod: '2024-03-13T22:44:52.812440-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u092E\u0947\u0902 CSV (Comma-Separated Values) \u092B\u093C\u093E\
  \u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\
  \u0930\u0928\u093E \u090F\u0938\u0921\u0940 \u0915\u093E\u0930\u094D\u0921 \u092A\
  \u0930 \u0938\u0902\u0917\u094D\u0930\u0939\u0940\u0924 \u0906\u092E\u0924\u094C\
  \u0930 \u092A\u0930 CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0938\u0947\
  \ \u092A\u0922\u093C\u0928\u0947 \u0914\u0930 \u0909\u0928\u092E\u0947\u0902 \u0932\
  \u093F\u0916\u0928\u0947 \u0915\u094B \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\
  \u0924\u093E \u0939\u0948, \u091C\u094B \u0921\u0947\u091F\u093E \u0932\u0949\u0917\
  \u093F\u0902\u0917, \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\
  \u0936\u0928 \u0938\u0947\u091F\u093F\u0902\u0917\u094D\u0938, \u0914\u0930 \u092C\
  \u0939\u0941\u0924 \u0915\u0941\u091B \u0938\u0915\u094D\u0937\u092E \u0915\u0930\
  \u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930 \u0905\u0915\u094D\u0938\u0930 \u0938\u0947\u0902\u0938\u0930 \u0921\
  \u0947\u091F\u093E \u0938\u0902\u0917\u094D\u0930\u0939, \u0915\u0949\u0928\u094D\
  \u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928 \u092A\u0948\u0930\u093E\u092E\u0940\
  \u091F\u0930 \u0938\u094D\u091F\u094B\u0930\u0947\u091C, \u092F\u093E \u0905\u0928\
  \u094D\u092F \u092A\u094D\u0930\u0923\u093E\u0932\u093F\u092F\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0907\u0902\u091F\u0930\u092B\u0947\u0938\u093F\u0902\
  \u0917 \u0915\u0947 \u0932\u093F\u090F CSV \u0915\u094B \u0939\u0948\u0902\u0921\
  \u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u0907\u0938\u0915\u0940 \u0938\u093E\u0926\u0917\u0940 \u0914\u0930\
  \ \u092A\u094D\u0932\u0947\u091F\u092B\u0949\u0930\u094D\u092E\u094B\u0902 \u092E\
  \u0947\u0902 \u0935\u094D\u092F\u093E\u092A\u0915 \u0938\u094D\u0935\u0940\u0915\
  \u0943\u0924\u093F \u0915\u0947 \u0915\u093E\u0930\u0923\u0964."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

## कैसे करें:
Arduino में CSV फ़ाइलों को हैंडल करने के लिए विशेष रूप से बनाई गई बिल्ट-इन लाइब्रेरी नहीं है, लेकिन आप एसडी कार्ड पर फ़ाइलों तक पहुँचने के लिए `SD` और `SPI` लाइब्रेरीज़ का उपयोग कर सकते हैं, और फिर CSV डेटा को पार्स करने या उत्पन्न करने के लिए बेसिक स्ट्रिंग मैनिपुलेशन तकनीकों का उपयोग कर सकते हैं। जब अधिक जटिल CSV मैनिपुलेशन का सामना करना पड़े, तो तीसरे पक्ष की लाइब्रेरी `ArduinoCSV` का उपयोग करके पार्सिंग और लिखने में आसानी हो सकती है।

**एसडी कार्ड से CSV डेटा पढ़ना:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("आरंभिकीकरण विफल!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // प्रिंट्स द CSV लाइन
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // इस उदाहरण में प्रयुक्त नहीं
}
```
*नमूना आउटपुट:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**एसडी कार्ड पर CSV डेटा लिखना:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("आरंभिकीकरण विफल!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSV हैडर
    dataFile.println("1, 1597840923, 23.5"); // उदाहरण डेटा रो
    dataFile.close();
    Serial.println("डेटा लिखा गया");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // इस उदाहरण में प्रयुक्त नहीं
}
```
*नमूना आउटपुट:*
```
डेटा लिखा गया
```

**पार्सिंग के लिए ArduinoCSV का उपयोग करना:**
यदि आप जटिल CSV फ़ाइलों का सामना कर रहे हैं, `ArduinoCSV` लाइब्रेरी पार्सिंग प्रयासों को काफी सरल बना सकती है। यह उदाहरण मानता है कि आपने पहले ही `ArduinoCSV` लाइब्रेरी स्थापित कर ली है।

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("आरंभिकीकरण विफल!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // प्रिंट प्रत्येक फील्ड
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // इस उदाहरण में प्रयुक्त नहीं
}
```
*नमूना आउटपुट:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
इन उदाहरणों में, एसडी कार्ड पर CSV फ़ाइलों से पढ़कर और उनमें लिखकर, Arduino प्रोजेक्ट्स आसानी से डेटा एकत्रित कर सकते हैं, कॉन्फ़िगरेशन सेटिंग्स स्टोर कर सकते हैं, या यूनिवर्सली सुलभ प्रारूप में अन्य अनुप्रयोगों के साथ डेटा का आदान-प्रदान कर सकते हैं।
