---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:29.214094-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जब हम किसी स्ट्रिंग को लोअर केस (छोटे अक्षरों) में बदलते हैं, तो हम उसके सभी अक्षरों को छोटा कर देते हैं। यह कदम डाटा को सामान्यीकृत करने के लिए, जैसे कि उपयोगकर्ता की इनपुट को एक निश्चित रूप में बदलना, और संवेदनशील (case-sensitive) तुलना से बचने के लिए लिया जाता है।

## How to: (कैसे करें:)
Arduino में एक स्ट्रिंग को लोअर केस में बदलने के लिए `toLowerCase()` फंक्शन का इस्तेमाल होता है। ये रहा एक साधारण उदाहरण:

```Arduino
void setup() {
  // सीरियल मॉनिटर शुरू करें
  Serial.begin(9600);
  
  // स्ट्रिंग बनाएं
  String myString = "Namaste, ARDUINO World!"; 
  
  // स्ट्रिंग को लोअर केस में बदलें
  myString.toLowerCase();
  
  // बदली गई स्ट्रिंग को प्रिंट करें
  Serial.println(myString);
}

void loop() {
  // यहाँ loop की जरूरत नहीं है
}
```

आउटपुट:
```
namaste, arduino world!
```

## Deep Dive (गहराई से जानकारी)
जब प्रोग्रामर्स लोअर केस में कन्वर्शन करते हैं, तो उन्हें हर अक्षर को इंडिविजुअली हैंडल करने की जरूरत पड़ती है। Arduino में `String` क्लास का `toLowerCase()` फंक्शन इसे आसान बनाता है। यह आधुनिक प्रोग्रामिंग भाषाओं के सामान्य फीचर्स में से एक है, पर ऐसा नहीं था हमेशा। पुराने सिस्टम्स में यह एक कठिन कार्य था।

विकल्प के रूप में, आप `for` लूप्स का इस्तेमाल करके मैन्युअली प्रत्येक अक्षर को लोअर केस में बदल सकते हैं। पर, यह अक्सर अनावश्यक जटिल होता है।

कन्वर्शन में मूल रूप से Unicode या ASCII मानों को व्यवस्थित किया जाता है, जहां हर बड़े अक्षर का एक संबंधित छोटा अक्षर मान होता है। अतः, `toLowerCase()` सामान्यत: हर अक्षर को उसके छोटे समकक्ष में परिवर्तित करता है।

## See Also (देखें भी)
- Arduino String Reference: [Arduino String toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- Unicode Standard: [Unicode](http://www.unicode.org/)
- ASCII Table and Description: [ASCII Table](http://www.asciitable.com/)

इन लिंक्स पर जा कर और अधिक जानकारी हासिल करें जो Arduino प्रोग्रामिंग और टेक्स्ट प्रोसेसिंग से संबंधित है।
