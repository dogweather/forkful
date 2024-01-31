---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:34:38.153424-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग से पार्स करना मतलब है कि आप टेक्स्ट फॉर्मेट में तारीख को पढ़कर उसे ऐसे ढांचे में बदलना जहाँ अर्दुइनो इसे समझ और प्रयोग कर सके। प्रोग्रामर्स इस प्रक्रिया का प्रयोग आंकड़ों को संग्रह करने, छांटने और अन्य कार्यों के लिए करते हैं।

## How to: (कैसे करें:)
```Arduino
// पहले उदाहरण के लिए निचे कोड देखें:
String dateString = "2023-03-15"; // YYYY-MM-DD format
int year, month, day;

void setup() {
  Serial.begin(9600); // सीरियल पोर्ट का आरंभ करें
  parseDate(dateString, year, month, day);
  Serial.print("Day: ");
  Serial.print(day);
  Serial.print(", Month: ");
  Serial.print(month);
  Serial.print(", Year: ");
  Serial.println(year);
}

void parseDate(String data, int &y, int &m, int &d) {
  y = data.substring(0, 4).toInt();
  m = data.substring(5, 7).toInt();
  d = data.substring(8, 10).toInt();
}

void loop() {
  // यहां कुछ नहीं करना है
}

// उदाहरण का आउटपुट यह होगा:
// Day: 15, Month: 3, Year: 2023
```

## Deep Dive (गहराई से जानकारी)
तारीख को पार्स करने का तरीका साधारण तो है, पर इसके विकास में अनेक परिष्कार शामिल हैं। पहले, डेटा प्रोसेसिंग में ASCII कोड्स और मैन्युअल पार्सिंग का इस्तेमाल होता था। आजकल, अधिकांश प्रोग्रामिंग भाषाओं में बिल्ट-इन फंक्शन्स और लाइब्रेरीज होती हैं जो यह काम सरल बनाते हैं। अर्दुइनो में स्ट्रिंग मेथड `substring()` और `toInt()` का प्रयोग करके इस तारीख को पार्स किया जा सकता है। वैकल्पिक रूप में, तारीख-विशिष्ट लाइब्रेरीज भी हैं जो इसे और भी आसान बना देते हैं, जैसे कि `TimeLib.h` या `DateTime.h`। 

प्रयोग में, विशेष ध्यान `delimiter` पर होना चाहिए, जैसा कि हाइफन (`-`) का उपयोग हमने यहाँ किया है। एक सही `delimiter` का चुनाव आपके डेटा फॉर्मेट पर निर्भर करेगा। हमेशा ऐसे फॉर्मेट का इस्तेमाल करें जो अंतर्राष्ट्रीय मानकों (ISO) के अनुरूप हों।

## See Also (और भी देखें)
- [Arduino Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)
- [Adafruit RTClib (Real Time Clock Library)](https://github.com/adafruit/RTClib)
