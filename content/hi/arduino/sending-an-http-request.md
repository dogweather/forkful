---
title:                "एक http अनुरोध भेजना"
html_title:           "Arduino: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# क्यों

एक Arduino में HTTP अनुरोध भेजना सीखना, आपको वेब सर्वर से डेटा को अपनी आईपी अनुमति से प्राप्त करने की अनुमति देता है। 

## कैसे करें

इसके लिए आपको स्केच कोड को वेब साइट के साथ जोड़ना होगा। नीचे दिए गए कोड ब्लॉक में, पुर्तगाली वेबसाइट से डेटा प्राप्त करने के लिए हम एक उदाहरण देंगे।

```Arduino
#include <SPI.h>
#include <WiFi.h>
#include <HttpClient.h>

char ssid[] = "YourNetworkSSID";
char pass[] = "YourNetworkPassword";

// Your Arduino's IP address
IPAddress ip(192, 168, 1, 177);

// Server IP address (in this example, a Portuguese website)
IPAddress server(78, 109, 175, 74);

// Create an instance of the HttpClient
WiFiClient client;
HttpClient http(client, server, 80);

void setup() {
  // Initialize serial communication
  Serial.begin(9600);

  // Connect to WiFi network
  WiFi.begin(ssid, pass);

  // Wait for connection to be established
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi...");
  }

  // Print connection status
  Serial.println("Connected to WiFi!");

  // Configure Arduino's IP
  WiFi.config(ip);
}

void loop() {
  // Send GET request to server and save response in a variable
  HttpResponse response = http.get("/");

  // Print response status code
  Serial.println(response.statusCode);

  // Print response body
  Serial.println(response.body);

  // Wait for 10 seconds before making another request
  delay(10000);
}
```

अब आपका स्केच कम्पाइल करके अपने Arduino बोर्ड पर अपलोड कर सकते हैं। जब आप स्केच को अपने Arduino में अपलोड करते हैं, तो मॉनिटरिंग से अभिंत भाग बनाएं और आईपी अनुमति का इस्तेमाल करके वेबसाइट से एक HTTP अनुरोध भेजेंगे। आप मॉनिटरिंग से कुछ इस तरह की आउटपुट देखेंगे:

```
Connecting to WiFi...
Connecting to WiFi...
Connecting to WiFi...
Connected to WiFi!
200
<!DOCTYPE html>
<!--[if lt IE 7]> <html class="ie ie6 lte9 lte8 lte7 os-win"> <![endif]-->
<!--[if IE 7]>    <html class="ie ie7 lte9 lte8 lte7 os-win"> <![endif]-->
<!--[if IE 8]>    <html class="ie ie8 lte9 lte8 os-win"> <![endif]-->
<!--[if IE 9]>    <html class="ie ie9 lte9 os-win"> <![endif]-->
<!--[if (gte IE 10)|!(IE)]><!--> <html class="os-win"> <!--<![endif]-->
...
</html>
```

आप उस वेबसाइट के हथियारों के साथ कुछ अन्य डेटा प्राप्त करने