---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीख को स्ट्रिंग में बदलना का क्या मतलब है और क्यों प्रोग्रामर ऐसा करते हैं? जब हम टाइम-स्टैम्प या दिनांक को इंसान-पठनीय रूप में परिवर्तित करते हैं, इसे हम स्ट्रिंग में बदलना कहते हैं। यह आइडेंटिफिकेशन, लॉगेस और डाटा विश्लेषण के लिए अत्यंत महत्वापुर्ण हो सकता है।

## कैसे करें:
सैम्पल कोड और आउटपुट नीचे दिया गया है:
```
Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 30, 40, 1, 1, 2020); // HH, MM, SS, DD, MM, YYYY
}

void loop() {
  time_t currentTime = now();
  String dateTime = String(hour(currentTime)) + ":" +
                    String(minute(currentTime)) + ":" +
                    String(second(currentTime)) + " " +
                    String(day(currentTime)) + "/" +
                    String(month(currentTime)) + "/" +
                    String(year(currentTime));

  Serial.println(dateTime);
  delay(1000);
}
```
उपरोक्त कोड का आउटपुट इस प्रकार होगा:
"10:30:40 1/1/2020"

## गहरा गोता
तारीख को स्ट्रिंग में बदलने का विचार उन समयों से सम्बंधित है जब कंप्यूटर की दुनिया में सांख्यिकीय तथ्यों की खोज हुई थी। वास्तव में, आज भी इंसान-पठनीय फ़ॉर्मॅट का इस्तेमाल बहुत आवश्यक और महत्वपूर्ण है। वैकल्पिक तरीके शामिल कर सकते हैं snprintf नामक C++ फंक्शन का उपयोग करना, जो एक बहुत ही छूटीली और शक्तिशाली इंस्ट्रुमेंट है। बेशक, अर्दुइनो हर्डवेयर पर नियंत्रण प्रदान करने वाले बिल्ट-इन फंक्शन्स होते हैं।

## अन्य स्रोतों के लिए देखें
आप [Arduino Time library documentation](https://www.arduino.cc/en/Reference.Time) और [snprintf function](http://www.cplusplus.com/reference/cstdio/snprintf/) के बारे में और जानने के लिये उनकी आधिकारिक वेबसाइट पर जा सकते हैं। हमेशा नए और अद्वितीय तरीके की खोज में रहें, जो आपकी कोडिंग को और अधिक उपयोगी और आसान बना सकते हैं। शुभकामनाएँ!