---
title:                "HTML विश्लेषण"
date:                  2024-02-03T19:12:53.880729-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

आर्डुइनो परियोजनाओं में HTML की पार्सिंग वेब पन्नों से जानकारी निकालने के बारे में होती है। प्रोग्रामर इसे इसलिए करते हैं ताकि वे अपने आर्डुइनो उपकरणों को इंटरनेट के साथ इंटरैक्ट करने में सक्षम बना सकें, वेबसाइटों से डेटा एकत्रित करके घर के ऑटोमेशन से लेकर पर्यावरणीय निगरानी तक के उद्देश्यों के लिए।

## कैसे:

आर्डुइनो पर HTML पार्सिंग आम तौर पर सीमित डिवाइस संसाधनों के कारण न्यूनतम फुटप्रिंट लाइब्रेरीज़ की मांग करती है। वेब स्क्रेपिंग और पार्सिंग के लिए एक लोकप्रिय विकल्प `ESP8266HTTPClient` और `ESP8266WiFi` लाइब्रेरीज़ का उपयोग करना है ESP8266 के लिए, या उनके ESP32 समकक्ष, दिए गए उनके मूल समर्थन के लिए वाई-फाई क्षमताओं और HTTP प्रोटोकॉल के लिए। यहाँ एक मूल उदाहरण है HTML लाने और पार्स करने के लिए, मानते हुए आप ESP8266 या ESP32 के साथ काम कर रहे हैं:

पहले, आवश्यक लाइब्रेरीज़ को शामिल करें:
```cpp
#include <ESP8266WiFi.h> // ESP8266 के लिए
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// यदि आप ESP32 का उपयोग कर रहे हैं तो समरूप ESP32 लाइब्रेरीज़ का उपयोग करें

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

अपने वाई-फाई नेटवर्क से जुड़ें:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Connecting...");
    }
}
```

एक HTTP अनुरोध बनाएं और एक सरल HTML टुकड़े को पार्स करें:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //वाई-फाई कनेक्शन स्थिति की जाँच करें
        HTTPClient http;  //HTTPClient क्लास की एक वस्तु की घोषणा

        http.begin("http://example.com");  //अनुरोध गंतव्य निर्दिष्ट करें
        int httpCode = http.GET();  //अनुरोध भेजें

        if (httpCode > 0) { //लौटने वाले कोड की जाँच करें
            String payload = http.getString();   //अनुरोध प्रतिक्रिया लोड प्राप्त करें
            Serial.println(payload);             //प्रतिक्रिया लोड मुद्रित करें

            // एक विशिष्ट भाग को पार्स करें, उदाहरण के लिए, शीर्षक निकालना लोड से
            int titleStart = payload.indexOf("<title>") + 7; // +7 टैग "<title>" से आगे बढ़ने के लिए
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Page Title: ");
            Serial.println(pageTitle);
        }

        http.end();   //कनेक्शन बंद करें
    }

    delay(10000); //हर 10 सेकंड में एक अनुरोध करें
}
```

नमूना आउटपुट (मानते हुए http://example.com में एक सरल HTML संरचना है):
```
Connecting...
...
Page Title: Example Domain
```

यह उदाहरण एक HTML पेज लाने और `<title>` टैग सामग्री निकालने का प्रदर्शन करता है। अधिक जटिल HTML पार्सिंग के लिए, स्मृति प्रतिबंधों के कारण सावधानी के साथ नियमित एक्सप्रेशन्स का उपयोग करने पर विचार करें या HTML संरचना के माध्यम से नेविगेट करने के लिए स्ट्रिंग मैनिप्यूलेशन फंक्शनों का उपयोग करें। उन्नत पार्सिंग अधिक जटिल दृष्टिकोणों की मांग कर सकती है, जिसमें आपके सामने आने वाले HTML की विशिष्ट संरचना के लिए तैयार की गई कस्टम पार्सिंग एल्गोरिदम शामिल हो सकते हैं, क्योंकि मानक आर्डुइनो वातावरण में एक निर्मित HTML पार्सिंग लाइब्रेरी शामिल नहीं है।
