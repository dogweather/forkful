---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP request भेजना, किसी web server से data को fetch करने का एक simple तरीका है। Programmers इसे तभी use करते हैं जब उन्हें अपने Arduino device को remotely control करना होता है या किसी बाहरी source से data प्राप्त करना होता है।

## कैसे करें:

Arduino HTTP request bhjene ke liye, ESP8266WiFi library ko use karte hain. Niche ek example diya gaya hai:

```Arduino
#include <ESP8266HTTPClient.h>
#include <ESP8266WiFi.h>

void setup() {
  WiFi.begin("your_SSID", "your_PASSWORD");

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
  }

  HTTPClient http;
  
  http.begin("http://example.com"); 
  int httpCode = http.GET();                                       
      
  if (httpCode > 0) {
    String payload = http.getString();    
    Serial.println(payload);
  }
  
  http.end(); 
}

void loop() {}
```
Example program में, हम्हे "your_SSID" और "your_Password" अपने WiFi network के details से replace करना होगा। इसके बाद, इस program को run करने से हमें "http://example.com" website का response mil jayega।

## गहराई में:
HTTP request का concept World Wide Web के early दिनों से ही use किया जा रहा है। Arduino में इसका implementation HTTPClient library के through होता है, जो की कई different types की HTTP methods supports करता है, जैसे की GET, POST, PUT, DELETE, etc। 

Arduino के alternatives में Particle, BeagleBone Black, और Raspberry Pi होते हैं। ये सभी boards internet-connected applications के लिए use किये जा सकते हैं, अलग-अलग boards का use different situations में होता है। जैसे की, Raspberry Pi में एक fully functional operating system hota है जो complex tasks के लिए suitable होता है। 

## और भी देखें:

- Arduino's HTTPClient Library documentation: [link](https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient/readme.html)
- More about HTTP requests: [link](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- Alternatives to Arduino: [Raspberry Pi](https://www.raspberrypi.org/), [BeagleBone](https://beagleboard.org/bone), [Particle](https://www.particle.io/)