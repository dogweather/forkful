---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फाइल लिखना मतलब डेटा को टेक्स्ट फॉर्मेट में सेव करना होता है। प्रोग्रामर्स यह इसलिए करते हैं ताकि डेटा को आसानी से स्टोर, ट्रांसफर, और रिकवर कर सकें।

## कैसे करें:
```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  myFile = SD.open("test.txt", FILE_WRITE);

  if (myFile) {
    myFile.println("नमस्ते अर्दुइनो!");
    myFile.close();
    Serial.println("फाइल लिखी गई।");
  } else {
    Serial.println("फाइल खोलने में असमर्थ।");
  }
}

void loop() {
  // यहाँ कुछ नहीं करना है
}
```

## गहराई से:
टेक्स्ट फाइलें लिखने का अभ्यास कंप्यूटर के प्रारंभिक दिनों से ही हो रहा है, जब टेप्स और पंच कार्ड्स का उपयोग होता था। आज, SD कार्ड्स और EEPROM जैसे मेमोरी सोल्यूशन्स हैं जिनका अर्दुइनो में इस्तेमाल किया जाता है। `SD` लाइब्रेरी `SPI` संचार का उपयोग करती है टेक्स्ट फाइलें लिखने के लिए, और काफी प्रभावशाली है क्योंकि डेटा लॉगिंग, कॉन्फिगरेशन फाइलें, और यूज़र इंटरफेस सेविंग के लिए इस्तेमाल होता है। 

## यह भी देखें:
- Arduino स्मार्ट डेटा लॉगिंग: https://www.arduino.cc/en/Tutorial/BuiltInExamples/Datalogger
- SD लाइब्रेरी रेफरेंस: https://www.arduino.cc/en/Reference/SD
- SPI कम्युनिकेशन पर जानकारी: https://www.arduino.cc/en/Reference/SPI
