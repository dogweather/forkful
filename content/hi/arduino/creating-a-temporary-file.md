---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:40:19.202241-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
प्रोग्रामर्स अस्थायी फ़ाइल बनाते हैं ताकि वे डेटा को अस्थायी रूप से स्टोर कर सकें और प्रोग्राम के चलते उसका उपयोग कर सकें। यह उपयोगी होता है जब डेटा की एक बार की जरूरत होती है और उसे स्थायी रूप से संग्रहित नहीं करना पड़ता।

## How to: (कैसे करें:)
Arduino में डायरेक्टली अस्थायी फाइल बनाने का कोई स्टैंडर्ड फीचर नहीं है, लेकिन हम SD कार्ड लाइब्रेरी का इस्तेमाल कर सीमित समय के लिए फाइल बना कर उसे अस्थायी फाइल के रूप में उपयोग कर सकते हैं।
```Arduino
#include <SD.h>
File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  tempFile = SD.open("temp.txt", FILE_WRITE);
  if (tempFile) {
    tempFile.println("This is a temporary file.");
    tempFile.close(); // यहाँ पर फ़ाइल को बंद कर दिया गया है
  } else {
    Serial.println("Error creating the file!");
  }
}

void loop() {
  // अपना कोड यहाँ लिखें
}
```
सैंपल आउटपुट: यदि फाइल सही से बन जाती है, तो SD कार्ड में "temp.txt" नामक फ़ाइल मिलेगी जिसमें "This is a temporary file." की एक पंक्ति होगी।

## Deep Dive (गहराई से जानकारी)
Arduino में अस्थायी फाइलें उतनी आम नहीं हैं जितनी कंप्यूटर प्रोग्राम्मिंग में। इतिहास में जब मेमोरी की कमी थी, अस्थायी फाइलें जरूरी थीं। आजकल, विशेष रूप से एम्बेडेड सिस्टम्स में, हम सीधे EEPROM या SD कार्ड को स्टोरेज के रूप में इस्तेमाल करते हैं। अस्थायी फाइलों का विकल्प है वैरिएबल्स या रैम में डेटा स्टोर करना। पर EEPROM लिखने की सीमित संख्या होती है और रैम की मात्रा भी कम होती है। 

अस्थायी फाइलें सबसे अच्छी होती हैं जब आपको बड़ी मात्रा में डेटा केवल कुछ समय के लिए संग्रहित करना हो, जैसे कि डाटा लॉगिंग के दौरान। इसे एक बार का उपयोग की गई फाइल के रूप में समझें जिसे आप उपयोग के बाद मिटा सकते हैं। 

## See Also (और भी देखें)
- SD Library for Arduino: https://www.arduino.cc/en/reference/SD
- EEPROM Write Limitations: https://www.arduino.cc/en/Tutorial/EEPROMWrite
- Memory Management in Arduino: https://www.arduino.cc/en/Guide/Memory
