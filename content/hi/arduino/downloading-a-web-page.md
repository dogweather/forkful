---
title:                "एक वेब पेज डाउनलोड करना।"
html_title:           "Arduino: एक वेब पेज डाउनलोड करना।"
simple_title:         "एक वेब पेज डाउनलोड करना।"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Arduino में वेब पेज डाउनलोड करना क्या है?

जब हम वेब पेज डाउनलोड करते हैं, तो हम इंटरनेट से डेटा को अपनी कंप्यूटर पर डाउनलोड करते हैं। प्रोग्रामर ये स्टेटिक डेटा के साथ प्राप्त करते हैं, जो पेज समझने के लिए ज़्यादा सपोर्ट नहीं करता। वे अपने प्रोग्राम में उस डेटा का उपयोग करते हैं जो पेज से लिया गया होता है।

## कैसे करे:

आपको पहले से ही अपने Arduino बोर्ड पर बूटलोडर को अपग्रेड करने की ज़रूरत होगी। तब आपको अपने Arduino में कुछ कोड जोड़ने की आवश्यकता है जो आपको वेब पेज डाउनलोड करने की सुविधा देगा।

```
ArduinoWiFiClient client;
if (client.connect("www.example.com", 80)) {
  client.println("GET /index.html HTTP/1.0");
  client.println("Host: www.example.com");
  client.println();
}
while (client.available()) {
  char c = client.read();
  Serial.write(c);
}
```

जब आप संदेश का जबाब पाते हैं, तो आपको उसे प्रिंट करने की आवश्यकता हो सकती है।

```
if (client.available() == 0) {
  client.stop();
}
```

## गहराई में पता करें:

इस फ़ंक्शन सही ढंग से काम करने के लिए, आपको अपने स्केलर का अपवाद करना होगा। अपने बोर्ड के साथ आपको JavaScript डाउनलोड करना होगा ताकि आप इसे अपने जीपीओनामिक्स कोड से काम कर सकें। सर्वर के साथ कनेक्ट करने में अन्य कठिनाइयां भी हो सकती हैं, जो आपको अपने कोड में सुधार करने की आवश्यकता हो सकती है।

## भीड़ भट्टा करें:

आपको इससे जुड़े सोर्सों पर और जानकारी प्राप्त करने के लिए Arduino वेबसाइट पर जाना होगा। आप अपने Arduino से वेब पेज डाउनलोड करने के लिए अन्य तरीके भी जान सकते हैं।

[अर्डुइनो वेबसाइट](https://www.arduino.cc/) 

[अर्डुइनो समुदाय](https://forum.arduino.cc/index.php) 

[अर्डुइनो डॉक्स (हिंदी)](https://www.arduino.cc/reference/hi)