---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Substring उत्तरण का मतलब होता है एक मूल वाक्यांश से एक छोटा वाक्यांश प्राप्त करना। यह एक महत्वपूर्ण कार्य है जब किसी विचरित डेटा के एक विशेष भाग की आवश्यकता होती है।

## कैसे करें:
```Arduino
String myString = "हेलो, दुनिया!";
String mySubstring = myString.substring(6, 12);
Serial.println(mySubstring);
```
ऊपरी कोड चलाने पर output दुनिया! होगा।

## गहराई में:
1) **ऐतिहासिक संदर्भ:** Arduino एक open-source प्लेटफॉर्म है, जो 2005 के करीब बनाई गई थी। प्रारंभ में, उप-वाक्यांश उत्तरण की क्षमता नहीं थी, लेकिन इसकी ओर महत्वपूर्ण मांग ने इसे जल्द ही जोड़ दिया।

2) **विकल्प:** आप split function का भी उपयोग कर सकते हैं, लेकिन यह एक तार हिट के बीच के characters बंद करता है। अगर आपको केवल string का एक विशेष भाग चाहिए, तब substring सबसे playing and simple विधियां है।

3) **आवेदन विवरण:** `substring()` function दो arguments लेता है: प्रारंभ और अंत का zero-based इंडेक्स, और यह इंडेक्स के बीच का वाक्यांश वापस देता है।

## आगे देखो:
Arduino के अन्य functions के बारे में जानने के लिए, नीचे दी गई links देखें:
- Arduino `indexOf()` Function: [Link](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- Arduino `charAt()` Function: [Link](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charat/)