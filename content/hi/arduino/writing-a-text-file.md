---
title:                "Arduino: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

यदि आप अपने अर्दुइनो बोर्ड पर डेटा को संग्रहित करना चाहते हैं तो आपको एक टेक्स्ट फाइल में स्टोर करने की आवश्यकता पड़ सकती है। इससे आप अपने डेटा को बाद में उपयोग कर सकते हैं या उसे किसी अन्य डिवाइस के साथ साझा कर सकते हैं। इसलिए, एक टेक्स्ट फाइल पर डेटा लिखना अपने अर्दुइनो प्रोग्रामिंग में अहम एक्शन हो सकता है।

## कैसे करे

टेक्स्ट फाइल पर डेटा लिखने के लिए आपको सबसे पहले एक फ़ाइल बनाना होगा। इसके बाद, आपको उस फाइल को ओपन करना होगा और डेटा को लिखना होगा। नीचे दिए गए कोड ब्लॉक में आप एक सरल डेटा लिखने का उदाहरण देख सकते हैं।

```Arduino
#include <SD.h>

File myFile;

void setup() {
  // SD कार्ड को इनिशलाइज़ करें
  Serial.begin(9600);
  Serial.print("Initializing SD card...");

  if (!SD.begin(10)) {
    Serial.println("असफल!");
    return;
  }
  Serial.println("शांत!");
```

अगर आपको केवल एक सरल टेक्स्ट फाइल बनानी है तो ऊपर दिए गए कोड से काफी होगा। लेकिन अधिक संदर्भीय डेटा को लिखने के लिए आपको थोड़ी सी गहन जानकारी को लिखने की आवश्यकता हो सकती है।

```Arduino
myFile = SD.open("sensorData.txt", FILE_WRITE);

if (myFile) {
  // sensorData.txt फ़ाइल में डेटा लिखें
  myFile.println("Temperature: 25");
  myFile.println("Humidity: 50");
  myFile.close();
  Serial.println("डेटा लिख दिया!");
} else {
  Serial.println("फ़ाइल नहीं खोल रहा है!");
}
```

ऊपर दिए गए उदाहरण में, हमने फाइल को "sensorData.txt" के रूप में बनाया और उसमें दो अलग-अ