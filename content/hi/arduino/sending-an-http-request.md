---
title:                "Arduino: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध भेजने में लोग क्यों शामिल होंगे? क्या वह जरूरी है? क्या यह आपके अर्डुइनो कोड में एक अहम Role निभाता है? इन सभी सवालों का जवाब हाँ है।

## कैसे

अगर आपको आज अपने अर्डुइनो बोर्ड से HTTP अनुरोध भेजना सीखना है तो आप सही जगह पर हैं। नीचे दी गई कोड उदाहरण आपको प्रारंभ करने में मदद करेंगे।

```Arduino
#include <ESP8266WiFi.h>

// Replace with your network credentials
const char* ssid = "MyWiFiNetwork";
const char* password = "MyWiFiPassword";

// Declare Server and Resource URLs
const char* server = "www.example.com";
const char* resource = "/";

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password); // Connect to WiFi network
  Serial.println("Connecting to WiFi...");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Connected to WiFi!");
}

void loop() {

  // Establish connection with server
  WiFiClient client;
  Serial.println("Connecting to server...");
  if (client.connect(server, 80)) {
    // Send HTTP request
    client.print(String("GET ") + resource + " HTTP/1.1\r\n" +
                 "Host: " + server + "\r\n" +
                 "Connection: close\r\n\r\n");

    // Read server response
    while(client.available()) {
      String line = client.readStringUntil('\r');
      Serial.print(line);
    }
    Serial.println();
  }
  else {
    Serial.println("Connection failed!");
  }
}
```

ऊपर दिए गए कोड ब्लॉक में `MyWiFiNetwork` और `MyWiFiPassword` अपनी वैफाई कनेक्शन के अनुसार बदलें। साथ ही `www.example.com` अपने सर्वर और `resource` को भी अपनी वेबसाइट की फ़ाइल के नाम से बदलें। इसके बाद अपने अर्डुइनो को लैप्टॉप से कनेक्ट करके कोड को अपलोड करें। इस्माताबहीन, अगर सब कुछ सही है तो आपको स्क्रीन पर कुछ Random टेक्स्ट दिखाई देगा, जो की आपकी इन्टरनेट ब्राउज़र की स्क्रीन पर भी दिखाई देगा। यह है HTTP अनुरोध प्रेषण का कामप्लीट कोड।

## डीप डाइव

HTTP अनुरोध लगभग हर वेबसाइट द्वारा उपभोग्य जानकार