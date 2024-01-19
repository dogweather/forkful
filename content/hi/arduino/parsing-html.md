---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

HTML पार्सिंग, इसने वेबपेज को खोलकर, उसे पढ़ने और मशीन-पढ़ने योग्य फॉर्मेट में संगठित करने का काम करता है। इसे प्रोग्रामर्स वेबपेज की संरचना को समझने, उसे रेकोड करने और विश्लेषित करने के लिए आता है।

## कैसे : 

निम्नलिखित Arduino कोड HTML कॉंटेंट को पार्स करने के उदाहरण के साथ :

```Arduino
#include <Arduino.h>
#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <ESP8266WebServer.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "नाम";
const char* password = "पासवर्ड";

void setup () {

  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {

    delay(1000);
    Serial.println("वाईफ़ाई से कनेक्ट करने का प्रयास ...");

  }

  Serial.println("वाईफ़ाई से जुड़ गया है।");
  Serial.println("आईपी पता: ");
  Serial.println(WiFi.localIP());
}

void loop() {

  if (WiFi.status() == WL_CONNECTED) { 

    WiFiClient client;
    HTTPClient http;  //Declare object of class HTTPClient

    http.begin("http://jsonplaceholder.typicode.com/comments?id=1");  //Specify request destination
    int httpCode = http.GET();  

    if (httpCode > 0) { 

      String payload = http.getString();
      Serial.println(payload);
    }

    http.end();   //Close connection
  }

  delay(5000);
}
```

## गहरी गोता 

1. ऐतिहासिक प्रासंगिकता : HTML पार्सिंग मशीनों के लिए वेब कंटेन्ट को और अधिक उपयोगी बनाता है।
2. विकल्प : सीधे मैन्युअल तरीके से डेटा एक्सट्रैक्ट करने की तुलना में, Python के BeautifulSoup और JavaScript के cheerio जैसे टूल्स का उपयोग करके HTML पार्स करनाकाफी आसान है। 
3. क्रियान्वयन विवरण : Arduino के प्रयोग द्वारा HTML पार्सिंग, HTTPClient और WiFiClient के क्लास का उपयोग कर वेबपेज से डेटा एक्सट्रैक्ट करता है।

## अधिक देखें 

1. Arduino कोडिंग गाइड : https://www.arduino.cc/reference/en/
2. BeautifulSoup और Cheerio का उपयोग करने का तरीका : https://realpython.com/beautiful-soup-web-scraper-python/
3. JSON Placeholder डेटा: https://jsonplaceholder.typicode.com/