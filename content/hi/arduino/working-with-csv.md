---
title:                "Arduino: कंप्यूटर प्रोग्रामिंग में सीएसवी के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में सीएसवी के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

**क्यों: CSV से काम करने से क्यों लोग लिपियों को लग सकते हैं?**

CSV (Comma-Separated Values) फाइलें डेटा को संग्रहीत करने का एक आसान तरीका है, जिससे उपयोगकर्ता डेटा को संपादित और व्यवस्थित कर सकते हैं। इसलिए, यदि आप अपनी Arduino प्रोजेक्ट में डेटा का प्रबंधन करना चाहते हैं, तो CSV फ़ाइलें आपके लिए बहुत सहायक हो सकती हैं।

**कैसे करें: CSV फ़ाइलों को Arduino पर लोड करना।**

इस उदाहरण में, हम अपने Arduino बोर्ड पर CSV फाइलों से डेटा लोड करने का एक सरल तरीका बताएंगे।

```arduino
#include <SPI.h>
#include <SD.h>

File dataFile;

void setup() {
  // SD कार्ड को शुरू करें
  Serial.begin(9600);
  while (!Serial) {;}

  Serial.print("Initializing SD card...");

  if (!SD.begin(4)) {
    Serial.println("खमोशी। SD कार्ड न मिला।");
    return;
  }
  Serial.println("सफलतापूर्वक संपादित होने पर:)");
}

void loop() {
  // यहां CSV फ़ाइल आपके SD कार्ड पर वहाँ स्थित होना चाहिए जहां आपने उसे रखा है।
  dataFile = SD.open("data.csv");

  // डेटा फ़ाइल से राउंड पढ़ें।
  while (dataFile.available()) {
    // प्रत्येक लाइन को पढ़ें और सीरियल पोर्ट पर मुद्दे को प्रिंट करें।
    Serial.write(dataFile.read());
  }
  // डेटा फ़ाइल को संकलित बंद करें
  dataFile.close();
  // 1 सेकंड के लिए रूकें
  delay(1000);
}
```

**गहराई में: CSV से काम करना**

CSV फाइलों का उपयोग डेटा को स्प्रेडशीट अनुपात में सजाने के लिए आसान बनाने के लिए किया जाता है। यह फाइलें टेक्स्ट फॉर्मेट में होती हैं और विभिन्न फ़ाइल फॉरमेटिंग समर्थन करती ह