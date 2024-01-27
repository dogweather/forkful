---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
स्ट्रिंग को कैपिटलाइज़ (Capitalizing a String) करना मतलब हर शब्द के पहले अक्षर को बड़ा (Uppercase) करना। प्रोग्रामर्स अक्सर यूज़र इंटरफेस में सामग्री की यूनिफार्मिटी और पठनीयता बढ़ाने के लिए ये करते हैं।

## कैसे करें? (How to:)
```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  String smallText = "arduino programming in hindi";
  String capitalizedText = capitalizeString(smallText);
  Serial.println(capitalizedText);
  delay(1000); // 1 Second Delay
}

String capitalizeString(String text) {
  if (text.length() == 0) {
    return "";
  }
  text.toLowerCase(); // पहले पूरी स्ट्रिंग को लोअरकेस में करें
  text[0] = text[0] - 32; // पहले अक्षर को कैपिटलाइज़ करें
  for (int i = 1; i < text.length(); i++) {
    if (text[i - 1] == ' ') {
      text[i] = text[i] - 32; // हर स्पेस के बाद वाले अक्षर को कैपिटलाइज़ करें
    }
  }
  return text;
}
```
सैंपल आउटपुट:
```
Arduino Programming In Hindi
```

## गहराई से जानकारी (Deep Dive)
कैपिटलाइज़ करना कम्प्यूटर प्रोग्रामिंग के पुराने जमाने से है, जब प्रिंटिंग प्रेस हुआ करती थी। यूनिकोड और ASCII अलग-अलग कैरेक्टर्स को डिफाइन करते हैं, और उनके उपयोग से स्ट्रिंग्स को कैपिटलाइज़ किया जाता है। उपरोक्त कोड में, हम अक्षरों के बीच स्पेस को चेक करके और 'A' से 'Z' तक के अक्षरों के ASCII मानों को मॉडिफाई करके कैपिटलाइज़ कर रहे हैं। इस काम के लिए अन्य लाइब्रेरीज और फंक्शन्स भी मौजूद हैं। 

## संबंधित स्रोत (See Also)
- [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII Table and Description](https://www.asciitable.com/)
- [Unicode Consortium](http://unicode.org/)
