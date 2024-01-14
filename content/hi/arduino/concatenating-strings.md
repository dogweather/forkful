---
title:                "Arduino: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

Arduino प्रोग्रामिंग का एक महत्वपूर्ण तत्व तारों को जोड़ने का है। यह विधि आपको दो या अधिक तर्कों को एक ही स्ट्रिंग में जोड़ने की अनुमति देती है, जो कि आपके स्केच को अधिक बढ़िया बनाता है।

## कैसे करें

Arduino में तारों को जोड़ने के लिए, हम `concat()` फ़ंक्शन का उपयोग करते हैं। इस फंक्शन को दो स्ट्रिंग्स के साथ कॉल किया जा सकता है और यह उन्हें एक ही स्ट्रिंग में जोड़ देगा। नीचे दिए गए उदाहरण में हम `firstString` और `secondString` नामक दो स्ट्रिंग्स को जोड़ रहे हैं।

```Arduino
String firstString = "Hello";
String secondString = "World";
String finalString = firstString.concat(secondString);
Serial.println(finalString);
// Output: HelloWorld
```

## गहराई में जाएं

Arduino में तारों को जोड़ने के लिए `concat()` फ़ंक्शन का उपयोग करना एक बहुत ही सरल और सटीक तरीका है। लेकिन, यदि आप एक अधिक उत्कृष्ट विकासक हैं, तो आप अपने स्केच में अन्य तरीकों से भी तारों को जोड़ सकते हैं। आप `+` ऑपरेटर का उपयोग करके भी स्ट्रिंग तारों को जोड़ सकते हैं।

```Arduino
String firstString = "Hello";
String secondString = "World";
String finalString = firstString + secondString;
Serial.println(finalString);
// Output: HelloWorld
```

## देखें भी

- [आर्दुइनो के साथ बच्चों को कोडिंग सिखाने के लिए 5 आसान तरीके](https://hindi.arduino.cc/blog/teaching-kids-coding-with-arduino/)
- [आर्दुइनो के साथ स्मार्ट होम बनाने के लिए सरल उपकरण](https://hindi.arduino.cc/blog/building-a-smart-home-with-arduino/)