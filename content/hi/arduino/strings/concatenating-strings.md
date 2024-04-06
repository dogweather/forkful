---
date: 2024-01-20 17:34:11.179384-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0940\
  \u0927\u0947 \u0938\u093F\u0902\u092A\u0932 \u092A\u094D\u0932\u0938 (`+`) \u0911\
  \u092A\u0930\u0947\u091F\u0930 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930 \u0915\u0947 strings join \u0915\u0930\u0928\u093E \u0938\
  \u0902\u092D\u0935 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u092A\u0930 `joinedString`\
  \ \u0926\u094B\u0928\u094B\u0902 string1 \u0914\u0930 string2 \u0915\u094B \u092E\
  \u093F\u0932\u093E\u0915\u0930\u2026"
lastmod: '2024-04-05T21:53:54.722616-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0940\u0927\u0947\
  \ \u0938\u093F\u0902\u092A\u0932 \u092A\u094D\u0932\u0938 (`+`) \u0911\u092A\u0930\
  \u0947\u091F\u0930 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930 \u0915\u0947 strings join \u0915\u0930\u0928\u093E \u0938\u0902\u092D\
  \u0935 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u092A\u0930 `joinedString` \u0926\
  \u094B\u0928\u094B\u0902 string1 \u0914\u0930 string2 \u0915\u094B \u092E\u093F\u0932\
  \u093E\u0915\u0930 \"Arduino \u0927\u092E\u093E\u0915\u093E\" \u092C\u0928\u093E\
  \u090F\u0917\u093E\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

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
