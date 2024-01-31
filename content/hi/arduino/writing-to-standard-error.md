---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
`Serial` लाइब्रेरी का उपयोग करके Arduino में डेटा को एरर के रूप में लिखना एक साधारण प्रक्रिया है; यह डिबगिंग के लिए जरूरी है. प्रोग्रामर्स इसे त्रुटियों को लॉग करने के लिए और मॉनिटरिंग के लिए करते हैं.

## How to: (कैसे करें:)
Arduino में स्टैंडर्ड एरर के लिए कोई विशेष चैनल नहीं है, लेकिन हम `Serial` का इस्तेमाल करके त्रुटियों को प्रिंट कर सकते हैं. निम्न कोड उदाहरण देखें:

```Arduino
void setup() {
  // सीरियल पोर्ट शुरू करें
  Serial.begin(9600);
}

void loop() {
  // कुछ चेक करें और एरर मैसेज भेजें अगर कुछ गलत हो
  if (somethingIsWrong) {
    Serial.println("ERROR: कुछ गलत हो गया है!");
  }
  // नियमित कोड...
}
```
अगर `somethingIsWrong` सत्य है, तो सीरियल मॉनिटर पर एरर मैसेज दिखाई देगा.

## Deep Dive (गहन जानकारी)
Arduino में स्थापित स्टैंडर्ड एरर स्ट्रीम नहीं होती, जैसे कि अन्य प्रोग्रामिंग भाषाओं में होता है. `Serial.println()` फंक्शन सबसे आम तरीका है त्रुटियों को डिस्प्ले करने का. विकल्प के रूप में, आप एलईडी जैसे हार्डवेयर इंडिकेटर्स का इस्तेमाल कर सकते हैं, या बाहरी सिस्टम्स के साथ कम्युनिकेशन के लिए एरर कोड्स भेज सकते हैं.

## See Also (अन्य महत्वपूर्ण सूत्र)
Arduino के Serial लाइब्रेरी पर अधिक जानकारी के लिए, इन लिंक्स को देखें:

1. [Arduino Official Serial Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
