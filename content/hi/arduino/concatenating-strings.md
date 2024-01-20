---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
मिलाना “Concatenation” टर्म का उपयोग स्ट्रिंग्स को जोड़ने (दो या अधिक) के संदर्भ में होता है। प्रोग्रामर इसे उपयोग करते हैं ताकि वे विभिन्न स्रोतों से प्राप्त डेटा को एक या अधिक बिंदुओं पर ज़ोड़ सकें।

## कैसे करें:
यहाँ Arduino कोड की एक उदाहरण है जहाँ CONCATENATE फ़ंक्शन के उपयोग को समझाया गया है:
```Arduino
String s1 = "नमस्ते";
String s2 = " दुनिया";
String s3 = s1 + s2; // Concatenation
Serial.println(s3); // Outputs "नमस्ते दुनिया"
```

## गहरी जानकारी:
(1) ऐतिहासिक संदर्भ में, स्ट्रिंग्स को concat करने की क्षमता बहुत पहले से ही प्रोग्रामिंग भाषाओं मेंमौजूद थी। (2) वैकल्पिक रूप से, आप "sprintf" या "snprintf" जैसे फ़ंक्शन का उपयोग कर सकते हैं जो अधिक जटिल फ़ॉर्मेटिंग की अनुमति देते हैं। (3) Concatenation को कार्यान्वित करने के पीछे बहुत सारी विस्तार और गठन विवरण होती हैं, जेसे BadRequestException.

## अन्य स्रोतों के लिए:
अधिक विवरण के लिए, आप इनका अन्वेषण कर सकते हैं:
1. Arduino स्ट्रिंग विवरण : [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
2. Arduino स्ट्रिंग एपीआई संदर्भ : [String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
3. sprintf और snprintf संदर्भ: [sprintf](http://www.cplusplus.com/reference/cstdio/sprintf/), [snprintf](http://www.cplusplus.com/reference/cstdio/snprintf/)