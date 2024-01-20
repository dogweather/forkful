---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
डीबग आउटपुट प्रिंट करना एक प्रोग्रामिंग कौशल है जिसमें हम कोड का विश्लेषण करते हैं। प्रोग्रामर इसे क्यों करते हैं? इसका उत्तर सादा और सीधा है - डीबगिंग यानी त्रुटियों का पता लगाना और सही करना। 

## कैसे:
Arduino कोड का एक उदाहरण देखें:
```Arduino
void setup() {
  Serial.begin(9600); // बौद रेट (bits/second) सेट करेंगे 9600 के साथ
}

void loop() {
  int sensorValue = analogRead(A0); // सेंसर मान पढ़ें
  Serial.println(sensorValue); // मान को Serial Monitor पर प्रिंट करेंगे
  delay(1000); // देरी करेंगे 1 सेकंड के लिए 
}
```
जैसा कि आप देख सकते हैं, `Serial.println(sensorValue);` लाइन डीबगगिंग आउटपुट प्रिंट करती है।

## गहराई में:
1. __ऐतिहासिक संदर्भ:__ पहले, प्रिंट स्टेटमेंट का उपयोग डीबगिंग में पर्याप्त था। Arduino के आविष्कार के बाद, माइक्रोकंट्रोलर पर विशेष डीबग सुविधाएं उपलब्ध कराई गईं। 

2. __विकल्प:__ डीबगिंग के अन्य उपकरण में ब्रेकपॉइंट, स्टेप-ओवर, स्टेप-इंटो और स्टेप-आउट शामिल हैं। 

3. __क्रियान्वयन विवरण:__ Arduino की `Serial.print()` और `Serial.println()` जैसी फ़ंक्शंस डीबग आउटपुट को USB या अन्य कनेक्टेड उपकरण पर प्रिंट करते हैं। 

## देखें भी:
- Arduino सीरियल कम्युनिकेशन: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- डीबगिंग कौशल: https://www.arduino.cc/en/Tutorial/LibraryExamples/DigitalReadSerial