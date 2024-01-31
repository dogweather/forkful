---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:54:04.863336-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-a-text-file.md"
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
