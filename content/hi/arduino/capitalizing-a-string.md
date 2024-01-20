---
title:                "स्ट्रिंग को बड़ा अक्षर में परिवर्तित करना"
html_title:           "Arduino: स्ट्रिंग को बड़ा अक्षर में परिवर्तित करना"
simple_title:         "स्ट्रिंग को बड़ा अक्षर में परिवर्तित करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Arduino Programming: एक वाक्यांश को बड़ा अक्षर बनाने का तरीका

## क्या और क्यों?
वाक्यांश को बड़ा अक्षर बनाना का अर्थ है कि हम एक स्ट्रिंग के सभी अक्षरों को अपरकेस (बड़े अक्षर) में परिवर्तित करते हैं। प्रोग्रामर्स इसे करते हैं क्योंकि यह कुछ समयों में उपयोगकर्ता की इनपुट को standardized फॉर्म में रखने में सहायता करता है। 

## कैसे करें:
Arduino में, हमें अपना विधि (method) स्वयं लिखना होगा, जैसे कि:

```Arduino
void stringToUpperCase(char* s) {
  while (*s) {
    *s = toupper(*s);
    s++;
  }
}

void setup() {
  Serial.begin(9600);
  char s[] = "Hello, world!";
  stringToUpperCase(s);
  Serial.println(s);
}

void loop() {
  // put your main code here, to run repeatedly:
}
```

उपरोक्त कोड का आउटपुट निम्न होगा:
```Arduino
HELLO, WORLD!
```

## गहराई में जानकारी:
(1) Arduino में आमतौर पर ऐसी function उपलब्ध नहीं है जो स्ट्रिंग को अपरकेस में परिवर्तित करती है। इसलिए, हमें अपनी कस्टम function बनानी होती है। 
(2) वैकल्पिक रूप से, हम भिन्न कुतुभों के libs का उपयोग कर सकते हैं जैसे कि String.h जो अतिरिक्त स्वामित्व (functionality) प्रदान करते हैं।
(3) उपरोक्त फ़ंक्शन सभी अक्षरों को एक-एक करके अपरकेस में परिवर्तित करता है, जो ASCII संकेतों का उपयोग करके किया जाता है।

## अन्य स्रोत:
1. [Arduino String Reference - arduino.cc](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [toupper Function - cplusplus.com](http://www.cplusplus.com/reference/cctype/toupper/)
3. [C Programming - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)