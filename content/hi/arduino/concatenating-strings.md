---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:11.179384-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स को जोड़ने से मतलब उन्हें एक साथ मिलाकर नई स्ट्रिंग बनाना होता है। प्रोग्रामर्स यह काम विभिन्न टेक्स्ट फ्रैगमेंट्स को एक साथ जोड़ ताकि मीनिंगफुल मैसेज या डाटा फॉर्मेट्स बना सकें।

## How to: (कैसे करें:)
```Arduino
// String डेक्लेरेशन
String string1 = "Arduino ";
String string2 = "धमाका";

// Concatenating strings
String joinedString = string1 + string2;

// Print करना
Serial.begin(9600);
Serial.println(joinedString); // Output: Arduino धमाका
```
सीधे सिंपल प्लस (`+`) ऑपरेटर का इस्तेमाल कर के strings join करना संभव है। यहाँ पर `joinedString` दोनों string1 और string2 को मिलाकर "Arduino धमाका" बनाएगा।

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग को जोड़ना प्रोग्रामिंग में अक्सर जरूरी होता है। पुराने ज़माने में, जब मेमोरी और प्रोसेसिंग पावर सीमित थी, स्ट्रिंग कॉनकेटेनेशन को कुशलता से करना महत्वपूर्ण था। Arduino में तो स्ट्रिंग्स को जोड़ने के लिए `+` ऑपरेटर इस्तेमाल होता है, लेकिन बड़े प्रोग्राम में इसका अधिक use मेमोरी फ्रैगमेंटेशन का कारण बन सकता है। इसके लिए `String.reserve()` का इस्तेमाल करना चाहिए ताकि मेमोरी में पहले से ही जगह आरक्षित की जा सके। कुछ अन्य विधियाँ जैसे कि `strcat()`, `sprintf()` इत्यादि C++ में उपलब्ध हैं, पर Arduino पर स्ट्रिंग्स के साथ काम करते समय `String` क्लास का उपयोग अधिक सुविधाजनक होता है।

## See Also (और भी देखें):
- Arduino की `String` क्लास डॉक्यूमेंटेशन: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- स्ट्रिंग्स कैसे काम करती हैं: [Arduino String Tutorial](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator)
- मेमोरी मैनेजमेंट टिप्स: [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)