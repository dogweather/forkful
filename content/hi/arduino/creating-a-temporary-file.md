---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अस्थायी फ़ाइल बनाना एक प्रक्रिया है जिसमें किसी स्थायी डिस्क स्थान में डाटा को स्थायी रूप से सहेजने के बिना, डाटा को कुछ समय के लिए सहेजा जाता है। कार्यक्रमकर्ताओं का यह करने का मुख्य कारण यह होता है कि यह अस्थायी डाटा का प्रबंधन कुशलता से करने में सहायक होता है और डिस्क स्थान का सही उपयोग करता है।

## कैसे:
```Arduino
#include <SD.h>
File tempFile;

void setup()
{
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("initialization failed!");
    return;
  }
  Serial.println("initialization done.");

  tempFile = SD.open("temp.txt", FILE_WRITE);

  if (tempFile) {
    tempFile.println("This is a temporary file");
    tempFile.close();
    Serial.println("Writing done.");
  } 
  else {
    Serial.println("Error opening temp.txt");
  }
}

void loop()
{
  // nothing here. 
}
```
यह कोड SD कार्ड पर "temp.txt" नामक एक अस्थायी फ़ाइल बनाता है और इसमें "This is a temporary file" लिखता है। यदि फ़ाइल ठीक से बन गई है, तो "Writing done." प्रिंट किया जाता है।

## गहराई में:
कंप्यूटर प्रोग्रामिंग की शुरुआत में, अस्थायी फ़ाइलों की आवश्यकता नहीं होती थी क्योंकि प्रोग्राम सीधे मेमोरी में चलते थे। समय के साथ, जैसे-जैसे प्रोग्राम एवं डाटा बड़े हुए, अस्थायी फ़ाइलें डाटा संग्रहित करने का एक अच्छा विकल्प बन गईं। 

अल्टरनेटिव्स में ऐरी ऑन-डिस्क डाटाबेस, इन-मेमोरी डेटाबेस, या कस्टम डेटा संग्रहण स्कीम शामिल हैं, पर वे सभी अपने-अपने लाभ और प्रतिस्पर्धाओं हैं।

अस्थायी फ़ाइलों के बारे में विस्तृत जानकारी के लिए, आप सीसीपीईआरटीबी (Temporary-File Services: Component Overview) पर जाकर जांच सकते हैं।

# #यह भी देखें:
1. Arduino SD Library: https://www.arduino.cc/en/reference/SD
2. Arduino File Handling: https://www.arduino.cc/en/tutorial/files

ये लिंक्स आपको Arduino के साथ फाइल हैंडलिंग और SD कार्ड लाइब्रेरी के बारे में अधिक जानकारी प्रदान करेंगी।