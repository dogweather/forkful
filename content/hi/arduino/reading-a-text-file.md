---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक टेक्स्ट फाइल पढ़ना, यह होता है कब एक प्रोग्राम एक फाइल से जानकारी उपयोग करता है। प्रोग्रामर्स इसे उपयोग में लेते हैं ताकि ईंटरनेट की सामग्री को अपने कार्यक्रम में उपयोग कर सकें।

## प्रयोग कैसे करें:

आर्दुइनो (वर्तमान संस्करण) का उपयोग करते हुए, टेक्स्ट फाइल को पढ़ना निम्नलिखित रूप में हो सकता है:

```Arduino
#include<SD.h>

void setup() 
{
  Serial.begin(9600);

  if (!SD.begin(4)) 
  {
    Serial.println("Initialization Failed");
    return;
  }
  
  File data = SD.open("test.txt");

  if (data) 
  {
    while (data.available()) 
    {
      Serial.write(data.read());
    }
    data.close();
  } 
  else  
  {
    Serial.println("Error Opening test.txt");
  }
}
void loop() 
{

}
```

## गहरा अध्ययन

1. ऐतिहासिक संदर्भ: टेक्स्ट फ़ाइलों की पढ़ाई विकसित की गई थी ताकि प्रोग्रामर्स बाहरी डेटा से काम कर सकें।
2. विकल्प: कई अन्य भाषाएँ जैसे कि Python और Java भी टेक्स्ट फ़ाइल पढ़ने के लिए उपयोग की जा सकती हैं।
3. कार्यान्वयन विवरण: आर्दुइनो पुस्तकालय 'SD' SD कार्ड उपयोग करते हुए फ़ाइल को पढ़ता है और 'Serial' संचार में इस्तेमाल करता है।

## यदि आप देखना चाहते हैं

1. [Arduino documentation on SD library](https://www.arduino.cc/en/Reference/SD)
2. [Guide on working with Files in Arduino](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/files)