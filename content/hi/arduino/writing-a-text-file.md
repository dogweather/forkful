---
title:    "Arduino: एक टेक्स्ट फाइल लिखना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों
कारण प्रचलित आर्दुइनो प्रोग्रामर्स अपने कोड को सोर्स कंट्रोल के साथ संरक्षित रखते हैं। इसलिए, टेक्स्ट फाइल में कोड लेखन उनके लिए उपयोगी हो सकता है, जो कि अपने प्रोजेक्ट्स को सुरक्षित रखना चाहते हैं।

## कैसे करें
टेक्स्ट फाइल में आर्दुइनो कोड लिखने के लिए, पहले टेक्स्ट फाइल लाइब्रेरी को स्केच में शामिल करें। फिर, फ़ाइल बफ़र और फाइल पॉइंटर को बनाएं और ```Serial``` आउटपुट रीडिंग को टेक्स्ट फाइल में लिखें। आप ```println()``` फ़ंक्शन का उपयोग करके नए लाइन में लिख सकते हैं। नीचे दिए गए उदाहरण सेक्शन में आप इसका संकलन देख सकते हैं।

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  while (!Serial) {
    ;
  }

  Serial.print("Initializing SD card...");

  if (!SD.begin(4)) {
    Serial.println("Card failed, or not present");
    return;
  }

  Serial.println("card initialized.");
}

void loop() {
  Serial.println("Enter a value:");
  while (!Serial.available());
  float val = Serial.parseFloat();

  myFile = SD.open("data.txt", FILE_WRITE);
  if (myFile) {
    myFile.println(val);
    myFile.close();
    Serial.println("Successfully wrote to file.");
  } else {
    Serial.println("Error opening file.");
  }
}

```

टेक्स्ट फाइल में आर्दुइनो कोड लिखने के साथ, आप अपने टेक्स्ट अद्वितीय नाम में लिख सकते हैं। इस विधान का आंतरिक कार्य गुप्त रूप से चलता है, इसलिए आपको अपने कंप्यूटर से संपर्क करने की आवश्यकता नहीं होगी।

## गहरी जाँच
सुरक्षित रूप से गहराई के लिए, आपको ```SD``` का उपयोग करके सुनिश्चित करना होगा कि आपका टेक्स्ट फ़ाइल मौजूद है। आप अपने