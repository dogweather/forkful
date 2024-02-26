---
date: 2024-01-20 17:54:04.863336-07:00
description: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\
  \u0922\u093C\u0928\u093E \u0905\u0930\u094D\u0926\u0941\u0907\u0928\u094B \u092E\
  \u0947\u0902 \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u0921\u0947\u091F\u093E\
  \ \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0915\
  \u093E\u092E \u0921\u093F\u0935\u093E\u0907\u0938 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928 \u0932\
  \u094B\u0921 \u0915\u0930\u0928\u0947, \u0932\u0949\u0917 \u0926\u0947\u0916\u0928\
  \u0947, \u092F\u093E \u092B\u093F\u0930\u2026"
lastmod: '2024-02-25T18:49:50.004020-07:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\
  \u093C\u0928\u093E \u0905\u0930\u094D\u0926\u0941\u0907\u0928\u094B \u092E\u0947\
  \u0902 \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u0921\u0947\u091F\u093E \u090F\
  \u0915\u094D\u0938\u0947\u0938 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\
  \u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0915\u093E\
  \u092E \u0921\u093F\u0935\u093E\u0907\u0938 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928 \u0932\u094B\u0921\
  \ \u0915\u0930\u0928\u0947, \u0932\u0949\u0917 \u0926\u0947\u0916\u0928\u0947, \u092F\
  \u093E \u092B\u093F\u0930\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ फ़ाइल को पढ़ना अर्दुइनो में फ़ाइल से डेटा एक्सेस करने की प्रक्रिया है। प्रोग्रामर्स यह काम डिवाइस के लिए कॉन्फ़िगरेशन लोड करने, लॉग देखने, या फिर सेंसर डेटा को ऑफ़लाइन एनालिसिस के लिए करते हैं।

## How to: (कैसे करें:)
```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  myFile = SD.open("example.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening file");
  }
}

void loop() {
  // Nothing to do here
}
```
उदाहरण आउटपुट:
```
Hello, Arduino!
Sensor data: 25.6C
```

## Deep Dive (गहराई से जानकारी)
अर्दुइनो में पाठ फ़ाइल पढ़ने की क्षमता उपयोगिता को काफी बढ़ा देती है। शुरुआत में, अर्दुइनो केवल सिंपल इनपुट और आउटपुट तक सीमित था, पर एसडी कार्ड मॉड्यूल का इस्तेमाल करके हम फ़ाइल सिस्टम तक पहुंच सकते हैं। विकल्प में SPIFFS या EEPROM जैसे सिस्टम हैं, लेकिन बड़े डेटा के लिए SD कार्ड प्रिफ़रेबल होता है। फ़ाइल पढ़ते समय ध्यान रखें की बफरिंग और कैशिंग सही से काम कर रहा हो ताकि डेटा लॉस न हो।

## See Also (और जानकारी)
- SD library reference: https://www.arduino.cc/en/Reference/SD
- File handling in Arduino: https://www.arduino.cc/en/Guide/Environment#toc11
- SPIFFS File System: https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html
- EEPROM library: https://www.arduino.cc/en/Reference/EEPROM
