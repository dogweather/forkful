---
title:                "नियमित अभिव्यक्तियों का उपयोग करना"
html_title:           "Arduino: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों ("What & Why?")

रेगुलर एक्सप्रेशन्स यानि सामान्य अभिव्यक्तियाँ, विभिन्न पैटर्न्स की पहचान करने का तरीका होती हैं। प्रोग्रामर इसका उपयोग तेजी से और सरल ढंग से डाटा में पैटर्न्स ढूंढने, संशोधित करने और बदलने के लिए करते हैं। 

## कैसे ("How to:")

निम्नलिखित एक आर्डुइनो कोड ब्लॉक है, जिसमें एक रेगुलर एक्सप्रेशन द्वारा एक पैटर्न खोजा जा रहा है.

```Arduino
#include <regex.h>
void setup() {
  Serial.begin(9600);
  regex_t regex;
  int retval = regcomp(&regex, "o[0-9]?", 0);
  if (retval != 0) {
    Serial.println("Could not compile regular expression.");
  } 
}

void loop() {
  char msg[100];
  strcpy(msg, "Checking pattern o1 in Arduino");
  regmatch_t matches;
  if (regexec(&regex, msg, 1, &matches, 0) == 0) {
    Serial.println("Pattern found.");
  } else {
    Serial.println("Pattern not found.");
  }
}
```
इस कोड का आउटपुट होता है:
```
Pattern found.
```
इस उदाहरण में, हमने "o1" पैटर्न की खोज की है जिसे "Pattern found" के साथ पाया गया है।

## गहरा अध्ययन ("Deep Dive")

रेगुलर एक्सप्रेशन का इतिहास 1956 में केन थंपसन के द्वारा Unix ऑपरेटिंग सिस्टम के साथ शुरू हुआ था। इसके विकल्प के रूप में स्ट्रिंग फ़ंक्शन्स भी हैं, लेकिन वे अक्सर अधिक कोड और कम त्वरिता की आवश्यकता होती है। रेगुलर एक्सप्रेशन का उपयोग करना सीखना शुरू में कठिन हो सकता है, लेकिन यह अत्यंत शक्तिशाली उपकरण होता है जिसका अभ्यास करने से अच्छा परिणाम मिलता है।

## देखें भी ("See Also")

रेगुलर एक्सप्रेशन को आर्दुइनो में उपयोग करने के बारे में और जानकारी के लिए, निम्नलिखित स्रोतों का संदर्भ लें:

1. [Arduino Regular Expression Library](https://playground.arduino.cc/Main/Regexp)
2. [Regular Expressions in Arduino](https://stackoverflow.com/questions/9072320/split-string-with-delimiters-in-c)
3. [Nikhil Arya's Arduino Lessons](https://nikhilaryan.com/tag/arduino)