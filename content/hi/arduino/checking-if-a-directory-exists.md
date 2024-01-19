---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
html_title:           "Arduino: डायरेक्टरी मौजूद है या नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?
एक डायरेक्टरी मौजूद है या नहीं इसकी जांच एक सामान्य कार्य हैं जो आवश्यकतानुसार आर्डुइनो प्रोग्राम में बदलाव कर सकते हैं। इससे उस डायरेक्टरी में किये गए किसी भी बदलाव ( जैसे -  फ़ाइल बनाना, संपादित करना, हटाना, आदि ) को ट्रैक किया जा सकता है।

## कैसे करें:
यहाँ आपको कोडिंग उदाहरण और नमूना आउटपुट मिलेगा:

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  if (SD.begin(4)) {
    Serial.println("SD card is ready to use.");
  } else {
    Serial.println("SD card initialization failed");
    return;
  }
  if (SD.exists("example.txt")) {
    Serial.println("example.txt exists.");
  } else {
    Serial.println("example.txt doesn't exist.");
  }
}

void loop() {
  
}

```
नमूना आउटपुट:
```
SD card is ready to use.
example.txt exists.
```

## गहरा अध्ययन:
आवश्यकतानुसार आर्डुइनो प्रोग्राम में किसी डिरेक्टरी की मौजूदगी की जांच का विचार पहली बार 1970 के दशक में बनते समय किया गया था। इसके विकल्प के रूप में, आप ऐसे मेथड भी उपयोग कर सकते हैं जो फ़ाइलों की सूची का निर्माण करते हैं और फिर इसकी जांच करते हैं कि विशेष फ़ाइल या डायरेक्टरी सूची में है या नहीं।

## भी देखें:
1. [आधिकारिक आर्डुइनो डॉक्यूमेंटेशन](https://www.arduino.cc/en/Reference/HomePage)
2. [आर्डुइनो कक्षा पुस्तकालय फ़ंक्शन्स](https://www.arduino.cc/en/Reference/SD)