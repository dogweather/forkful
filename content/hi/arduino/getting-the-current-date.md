---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Arduino: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Arduino में हाल की तारीख प्राप्त कैसे करें: एक सरल गाइड

## आप क्या हैं और इसे क्यों करते हैं?
हाल की तारीख प्राप्त करना एक प्रोग्रामर के लिए बहुत महत्वपूर्ण है। यह उन्हें इन दिनों से जुड़े हुए तथ्यों को जानने में मदद करता है और अपनी प्रोग्राम को सही तारीख को आधार बनाकर समय सेट करने में भी सहायता प्रदान करता है। 

## कैसे करें:
```Arduino
#include <Time.h>
#include <TimeLib.h>

void setup(){
  setTime(16,26,00,23,4,2021);  //समय और तारीख को सेट करें
  Serial.begin(9600); //सीरियल मॉनिटर की शुरूआत करें
}

void loop(){
  time_t t = now(); 
  //वर्तमान समय और तारीख को प्रिंट करें
  Serial.print("वर्तमान समय: "); Serial.print(hour()); 
  Serial.print(":"); 
  printDigits(minute()); 
  Serial.print(":"); 
  printDigits(second()); 
  Serial.print("  वर्तमान तिथि: "); 
  Serial.print(day()); 
  Serial.print("/"); 
  Serial.print(month()); 
  Serial.print("/"); 
  Serial.println(year()); 
  delay(1000); 
} 

void printDigits(int digits){
  //दो अंकों के लिए 0 जोड़ें
  Serial.print(":"); 
  if(digits < 10) 
    Serial.print('0'); 
  Serial.print(digits); 
}
```
#### समाउत्पाद उदाहरण:
```
वर्तमान समय: 16:26:00
वर्तमान तिथि: 23/4/2021
```

## गहराई तक जाएं:
समय और तारीख को प्राप्त करने की इस प्रकार की सुलभता को विकसित किया गया था कि यह अब Arduino प्रोग्रामिंग में महत्वपूर्ण हो गया है। इसका विकल्प इतना स्पष्ट नहीं है। आप एक RTC (Real Time Clock) मॉड्यूल का उपयोग भी कर सकते हैं जो समय और तारीख को सीधे प्राप्त कर सकता है। 

## जुड़े रहें:
समय और तारीख को प्राप्त करने के लिए अधिक जानकारी के लिए निम्नलिखित स्रोतों की जाँच करें:

- [Time Library for Arduino](https://playground.arduino.cc/Code/time/)
- [Arduino Real Time Clock Tutorial](https://www.circuitbasics.com/how-to-set-up-an-i2c-rtc-for-arduino-and-esp8266/)