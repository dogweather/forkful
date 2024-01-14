---
title:                "Arduino: स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप एक आर्दुइनो प्रोग्रामर हैं और आपको उपयोगकर्ता से दर्ज किए गए डेटा को लोअर केस में परिवर्तित करने की जरूरत है, तो आप इस तकनीक का उपयोग कर सकते हैं। लोअर केस में दर्ज किया हुआ डेटा आर्दुइनो कोडिंग में आसानी से प्रोसेस किया जा सकता है।

## कैसे करें

आर्दुइनो में एक स्ट्रिंग को लोअर केस में परिवर्तित करने के लिए, हम `toLowerCase()` फंक्शन का उपयोग करेंगे। नीचे दिए गए कोड ब्लॉक में हम एक स्ट्रिंग का उचित तरीके से लोअर केस में परिवर्तन करते हैं।

```Arduino
String str = "HELLO WORLD";
String lowerStr = str.toLowerCase();
Serial.println(lowerStr);
```

आउटपुट:

```
hello world
```

## गहरी खड़ी

जब हम `toLowerCase()` फंक्शन का उपयोग करते हैं, तो आर्डुइनो एक स्ट्रिंग को लेकर विशेष रूप से काम करता है। यह स्ट्रिंग को बदलने के बजाय एक नई स्ट्रिंग को रिटर्न करता है। इसलिए, हमें एक अलग स्ट्रिंग में परिवर्तित वापसी संख्या प्राप्त करने के लिए इस नए स्ट्रिंग को संग्रह रखने की आवश्यकता होती है। यदि हम इसे स्ट्रिंग के साथ सीधे उपयोग करते हैं, तो हमारा उपयोगकर्ता मूल स्ट्रिंग को लोअर केस में परिवर्तित नहीं कर पाएगा।

## See Also

- [Official reference for `toLowerCase()` function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Tutorial on converting string to lower case in Arduino](https://www.circuitbasics.com/arduino-tutorial-how-to-use-the-lowercase-strings-function/)