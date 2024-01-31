---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
date:                  2024-01-19
html_title:           "Arduino: यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्ट्री मौजूद है या नहीं की जाँच करना, यह सुनिश्चित करने का एक तरीका है कि आप जिस डेटा को पढ़ना या लिखना चाहते हैं, उसके लिए स्थान है या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह एरर्स को रोकता है और सॉफ्टवेयर को ज्यादा विश्वसनीय बनाता है।

## How to: (कैसे करें:)
```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ;  // इंतजार करें जब तक सीरियल पोर्ट सक्रिय न हो जाए
  }

  if (!SD.begin(4)) {
    Serial.println("SD कार्ड इनिशियलाइजेशन फैल!");
    return;   // SD कार्ड शुरू नहीं हो पाया, लूप को रोकें
  }

  File root = SD.open("/");
  if (root.isDirectory()) {
    Serial.println("रूट डायरेक्ट्री मौजूद है!");
  } else {
    Serial.println("रूट डायरेक्ट्री नहीं मिली।"); 
  }
}

void loop() {
  // कुछ नहीं करना है
}
```
सैम्पल आउटपुट:
```
रूट डायरेक्ट्री मौजूद है!
```

## Deep Dive (गहराई से समझिए)
एसडी कार्ड और फाइल सिस्टम के साथ काम करते वक्त, Arduino पुराने समय से `SD.h` लाइब्रेरी का इस्तेमाल करता आ रहा है। इसके अलावा, `SdFat` लाइब्रेरी भी है जो कि ज्यादा एडवांस्ड फंकशन्स पेश करती है। जब हम डायरेक्ट्री की जाँच करते हैं, हमें यकीन हो जाता है कि कोई विशेष फाइल पथ एक्सेस करने लायक है या नहीं, जो कि डेटा इंटीग्रिटी और प्रोग्राम की दक्षता को सुनिश्चित करता है।

इस पद्धति का उपयोग करते हुए, हम यह भी चेक कर सकते हैं कि कोई नई डायरेक्ट्री बनानी है या नहीं, और यदि ज़रूरत हो, तो `SD.mkdir("/path/to/dir")` कोड का इस्तेमाल करके एक डायरेक्ट्री बना सकते हैं।

## See Also (और भी जानकारी)
- Arduino SD library documentation: https://www.arduino.cc/en/Reference/SD
- SdFat library GitHub repository: https://github.com/greiman/SdFat
- Arduino Forums for discussions and troubleshooting: http://forum.arduino.cc
