---
title:                "Arduino: कंप्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "कंप्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

क्या आपने कभी सोचा है कि आप कोड से कितना कुछ सीख सकते हैं? Arduino के रूप में एक प्रोग्रामिंग आपके कोडिंग कौशल को एक नई ऊंचाई पर ले जाता है। आज हम आपको बताएंगे कि कैसे आप अपने Arduino बोर्ड से एक टेक्स्ट फाइल पढ़ सकते हैं। इससे आप अपने स्केच को और भी मजेदार बना सकते हैं और अपने कोडिंग ज्ञान को बढ़ा सकते हैं।

## इसे कैसे करें

Arduino में टेक्स्ट फाइल पढ़ने के लिए आपको एक लाइब्रेरी इनके शेल आवश्यक होता है। आपको इसे  download करके Arduino IDE में जोड़ना होगा। साथ ही आपको अपने Arduino बोर्ड पर सीरियल पोर्ट को खोलना होगा।

```arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {

  Serial.begin (9600);
  while (!Serial) {}

  Serial.print("Initializing SD card...");
  if (!SD.begin(4)) { //4 is the CS pin on an arduino nano
    Serial.println("SD failed");
    return;
  }
  Serial.println("SD OK");
  
  myFile = SD.open("data.txt"); //open text file
  if (!myFile) {
    Serial.println("File not found"); // in case file is not present
  }
  
  while (myFile.available()) {
    Serial.write(myFile.read()); //read text file and print to serial monitor
  }
  
  myFile.close(); //close file
}

void loop() {
}
```

जब आप यह कोड स्केच अपने Arduino बोर्ड पर अपलोड करेंगे तो आपको सीरियल मॉनिटर पर टेक्स्ट फाइल का सारांश दिखाई देगा।

## गहराई में जाओ

सीरियल पोर्ट को खोलने के लिए  #4 मात्र एक महत्वपूर्ण उपाय है। जब आपको फाइल पाठी से जुड़ा होता है, तो आप इसे अपने स्केच में शामिल कर सकते हैं। कृपया ध्यान दें कि आपके स्केच के साथ सीरियल नंतर कोई इंटरफेस के रूप में आगे बढ़ने पर संभावना होसकती ह