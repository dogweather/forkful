---
title:                "वेब पेज डाउनलोड करना"
aliases:
- hi/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:54.882072-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करने का मतलब है इंटरनेट से HTML डाटा को पढ़ना और उसे सेव करना। प्रोग्रामर्स इसे डाटा एकत्रित करने, मॉनिटरिंग या वेब बेस्ड सेवाओं से जानकारी लेने के लिए करते हैं।

## How to: (कैसे करें?)
```Arduino
#include <SPI.h>
#include <Ethernet.h>

// MAC address और IP address को अपने नेटवर्क के अनुसार सेट करें.
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress ip(192, 168, 1, 100);

EthernetClient client;

void setup() {
  Ethernet.begin(mac, ip);
  Serial.begin(9600);
  
  // वेबसर्वर के लिए कनेक्ट होना.
  if (client.connect("example.com", 80)) {
    client.println("GET /path/to/page.html HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  } else {
    Serial.println("Connection failed");
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  if (!client.connected()) {
    client.stop();
  }
}
```
उपरोक्त कोड वेब पेज की सामग्री पढ़कर Serial Monitor पर प्रिंट करेगा।

## Deep Dive (गहराई में जानकारी)
इंटरनेट के शुरुआती दिनों में, वेब पेज डाउनलोड करना जटिल था। पुराने तरीके जैसे कि Telnet और FTP के बदले, आज HTTP और इसके secure संस्करण HTTPS का इस्तेमाल होता है। सरल Arduino लाइब्रेरीज जैसे Ethernet और WiFi इसे आसान बनाते हैं।

इसके विकल्प में, आप HttpClient या WiFiNINA जैसे और भी libraries इस्तेमाल कर सकते हैं, जो अधिक कार्यक्षमता प्रदान करते हैं। ध्यान रखें कि Web पेज डाउनलोड करते समय आपको HTTP headers और response को सही ढंग से handle करना होगा।

## See Also (और देखें)
- Arduino Ethernet Library Documentation: https://www.arduino.cc/en/Reference/Ethernet
- Ethernet Shield Getting Started Guide: https://www.arduino.cc/en/Guide/ArduinoEthernetShield
- Arduino HttpClient Library: https://www.arduino.cc/en/ArduinoHttpClient/Reference
