---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:37.304699-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डिबग आउटपुट प्रिंट करना एक ऐसा तरीका है जिससे प्रोग्रामर्स खोजी कर सकते हैं कि उनका कोड क्या कर रहा है। इससे एरर्स को खोजने और समझने में आसानी होती है।

## How To: (कैसे करें:)

Arduino में डिबग आउटपुट को Serial Monitor पर प्रिंट करने के लिए नीचे का कोड इस्तेमाल करें:

```Arduino
void setup() {
  // Serial पोर्ट को 9600 baud rate पर शुरू करें
  Serial.begin(9600);
}

void loop() {
  // वेरिएबल और अन्य जानकारी प्रिंट करें
  Serial.println("Hello, Debug!");

  // थोड़ी देर इंतजार करें
  delay(1000);
}
```

ऊपर का कोड हर सेकंड Serial Monitor पर "Hello, Debug!" प्रिंट करता रहेगा।

## Deep Dive (गहन जानकारी)

डिबग आउटपुट का इतिहास उतना ही पुराना है जितना कंप्यूटर प्रोग्रामिंग का। शुरूआती दिनों में, प्रोग्रामर्स लाइट्स और स्विचेस का इस्तेमाल करके मशीन की स्थिति का पता लगाते थे। आज, हम Serial.print() जैसे फंक्शंस का उपयोग करके Arduino का डिबग आउटपुट कंसोल पर देखते हैं।

Serial.print() के अलावा, एक अन्य तरीका है LCD डिस्पले या LEDs का उपयोग करना जो मदद कर सकता है जब आपके पास Serial Monitor तक पहुँच न हो। इम्प्लीमेंटेशन के विवरण में, हर Serial.println() कॉल सिस्टम की प्रोसेसिंग को थोड़ा धीमा कर सकती है, इसलिए ओप्टिमाइजेशन के लिए इसका सोच-समझकर इस्तेमाल करना और सटीक समय पर इसे निकाल देना जरूरी है।

## See Also (और जानिए)

- [Arduino's Serial.println reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Debugging Techniques for Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- [Optimizing Arduino Code for Performance](https://www.arduino.cc/en/Guide/ArduinoUno)
