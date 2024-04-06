---
date: 2024-01-20 17:54:04.863336-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.766379-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

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
