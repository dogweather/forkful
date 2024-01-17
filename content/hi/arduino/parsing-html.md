---
title:                "हैंडिंग हैटम्ल"
html_title:           "Arduino: हैंडिंग हैटम्ल"
simple_title:         "हैंडिंग हैटम्ल"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग क्या है और क्यों प्रोग्रामर इसे करते हैं? यह एक तरीका है जिससे हम वेबपेज्स से डेटा को निकाल सकते हैं और उसे अपने प्रोग्राम में उपयोग कर सकते हैं।

## कैसे:
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

void setup() {
  // Wi-Fi कनेक्शन सेट करें
  WiFi.begin("wifi_स्थानीय", "पासवर्ड");

  // सर्वर से डेटा डाउनलोड करें
  HTTPClient http;
  http.begin("https://example.com/");
  int httpCode = http.GET();

  // डाउनलोड किए गए डेटा को पार्स करें
  String data = http.getString();

  // समझें कि आप चाहते हैं कि स्ट्रिंग किसी HTML एलिमेंट से प्राप्त हो
  // उदाहरण के लिए: <p id="name">Arduino लेख निर्माण विधि</p> 
  // हमें प्रकाशित लेख के साथ बहुत सारे प्रकार के डेटा को निकालना है जो हमें स्ट्रिंग <p id="name"> से ऊपर लेते हैं। 
  // इसके लिए, हम अपनी स्ट्रिंग को split() फंक्शन का उपयोग करके पार्स करते हैं। 
  // हम अपने डेटा को कोई भी दिया गया अलगाव-friendly ठस्कहेरा मानता हूँ 
  // उदाहरण के लिए: <p id="name"> 
  String result = data.split("<p id=\"name\">")[1].split("</p>")[0]; 

  // प्रिंट करें
  Serial.println(result);
}

void loop() {}

```

आउटपुट: "Arduino लेख निर्माण विधि"

## गहराई में जाएं:
इतिहासी परिस्थिति: HTML का अस्तित्व 1993 में टिम बर्नर्स-ली द्वारा दिया गया था। 
वैकल्पिक: HTML पार्सिंग के लिए अन्य तरीके भी हैं जैसे कि एक्सप्रेस्स और जावास्क्रिप्ट पर आधारित पुरोहित आदि। 
अंतर्जाल प्रकरण: यह समझने के लिए महत्वपूर्ण है कि आप स्ट्रिंग को किस तत्व से प्राप्त करना चाहते हैं। 
इसके लिए आपको समान और असमान के अलावा कुछ और विकल्प हैं।

## भी देखें:
- [ESP8266WiFi Library](https://github.com/esp8266/Arduino)
- [Arduino स्प्लिट फंक्शन](https://www.arduino.cc/reference/en/language/functions/strings/string/split/)
- [डेटा पार्सिंग के लिए कुछ और अल्गाव-friendly ठस्क हैं](https://www.arduino.cc/reference/en/language/functions/communication/serial/parseint/)