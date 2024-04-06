---
date: 2024-01-20 17:52:37.304699-07:00
description: "How To: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Arduino\
  \ \u092E\u0947\u0902 \u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F\
  \ \u0915\u094B Serial Monitor \u092A\u0930 \u092A\u094D\u0930\u093F\u0902\u091F\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0928\u0940\u091A\u0947\
  \ \u0915\u093E \u0915\u094B\u0921 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930\u0947\u0902."
lastmod: '2024-04-05T21:53:54.738224-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Arduino \u092E\u0947\
  \u0902 \u0921\u093F\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u094B\
  \ Serial Monitor \u092A\u0930 \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0928\u0940\u091A\u0947 \u0915\u093E \u0915\
  \u094B\u0921 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0947\
  \u0902."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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
