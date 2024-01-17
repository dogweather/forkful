---
title:                "CSV के साथ काम करना"
html_title:           "Arduino: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV का उपयोग डेटा को स्प्रेडशीट जैसे स्ट्रक्चर में संग्रहीत करने के लिए गर्हित होता है। प्रोग्रामर्स इसका उपयोग करके बड़े उपायोगिताओं के साथ डेटा को एकल फॉर्मेट में संगठित कर सकते हैं।

## कैसे करें:
```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile; // फ़ाइल ऑब्जेक्ट बनाएं

void setup() {
  Serial.begin(9600); // सीरियल सतरंगी प्रिंट करने के लिए सेट अप करें
  if (!SD.begin(4)) { // अगर एसडी कार्ड नहीं मिला, तो सतरंगी मूल्य छोड़ें
    Serial.println("एसडी कार्ड से कनेक्ट नहीं किया गया");
    return;
  }
  dataFile = SD.open("data.csv"); // फ़ाइल को सच्ची करें और CSV फ़ॉर्मेट से डेटा पढ़ें
  if (dataFile) {
    while (dataFile.available()) { // सभी डेटा प्रिंट करने के लिए जब तक डेटा उपलब्ध हो
      Serial.print(dataFile.read());
    }
  dataFile.close(); // खोले गए फ़ाइल को बंद करें
  }
  else {
    Serial.println("फ़ाइल खोलने में असफल");
  }
}

void loop() {}

```

## टिप्पणी:
दूरस्थ सूचना प्रपात करने के लिए CSV का उपयोग 1972 में फ़ॉर्मेटिंग प्रारूप के रूप में पहली बार गर्हित हुआ। वैकल्पिक उपायोगिताओं में JSON, XML, और डेटाबेस आदि शामिल हैं।

## अध्ययन भी:
- [CSV शुरू करने के लिए Arduino लाइब्रेरी](https://github.com/tonyomy/CSV)
- [CSV के बारे में और अधिक जानकारी के लिए](https://www.theserverside.com/definition/CSV-Comma-Separated-Values)
- [CSV से कैसे फ़ाइल खोलें और पढ़ें](https://www.arduino.cc/en/Reference/SDopen)