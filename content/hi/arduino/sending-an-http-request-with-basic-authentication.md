---
title:                "Arduino: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों
यहां हम आपको बताएंगे कि आप क्यों एक Arduino बोर्ड का उपयोग करके HTTP अनुरोध भेजने में बेसिक प्रमाणीकरण के साथ लग सकते हैं। इसका उपयोग अपने आवासीय या कार्यालय नेटवर्क में एक्‍सेस करने के लिए, डेटा भेजने या प्राप्त करने के लिए फायरवॉल की जांच आवश्‍यक हो जाती है।

# कैसे करें
आप निम्नलिखित कोड ब्लॉक के भीतर दिए गए कोड संग्रह का उपयोग करके HTTP अनुरोध भेजने में बेसिक प्रमाणीकरण का उपयोग कर सकते हैं। इसके अलावा, आपको सेवा प्रदाता द्वारा प्रदान किए गए उपयोगकर्ता नाम और कुंजिका को अपने स्केच में सहेजना आवश्यक होगा।

```Arduino
#include <WiFiClient.h>
#include <ESP8266WiFi.h>
#include <WiFiClientSecure.h>

// WiFi और नेटवर्क सेटिंग्स कॉन्फ़िगर करने के लिए
char ssid[] = "अपना नेटवर्क नाम";
char pass[] = "अपना नेटवर्क पासवर्ड";
char server[] = "एपीआई का पता";
WiFiClientSecure client;

// उपयोगकर्ता नाम और कुंजिका को सेव करने के लिए टोकन
String auth_token = "आपका उपयोगकर्ता नाम:कुंजिका";

void setup() {
  // वाईफ़ी सेटिंग शुरू करें
  WiFi.begin(ssid, pass);
  // सफलतापूर्वक जुड़ने तक लूप में रहें
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }

  Serial.begin(9600);

  // सेवा प्रदाता से कनेक्ट करें
  if (!client.connect(server, 443)) {
    Serial.println("कनेक्ट करने में विफल: कनेक्शन विफल हुआ");
  }

  // हेडर में बेसिक प्रमाणीकरण जोडि़े
  String header = "Authorization: Basic " + auth_token + "\r\n" + "Content-Type: application/json" + "\r\n";

  // कहां कहां आ