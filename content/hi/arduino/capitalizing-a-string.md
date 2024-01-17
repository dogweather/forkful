---
title:                "स्ट्रिंग केपिटलाइज़ करना"
html_title:           "Arduino: स्ट्रिंग केपिटलाइज़ करना"
simple_title:         "स्ट्रिंग केपिटलाइज़ करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डिजिटल कंप्यूटिंग में, 'अर्द्ध/कैमेल केस' (यानी प्रथम अक्षर छोटे और बाकी मूल सारे अक्षर कपिटल) स्ट्रिंग को 'अधिकांश' (सभी अक्षर कैपिटल) में बदलना इस्तेमाल किया जाता है। यह दिखाता है कि स्ट्रिंग में शामिल सभी शब्द या शब्दांश स्थानांतरित हैं। कुछ समय के बाद, प्रोग्रामर बड़ी सारी स्ट्रिंग्स एक ही तरीके से देखना पसंद करते हैं।

## कैसे करे:

अर्दुइनो की फ़क्ट्री किताबतोर प्रदान किया गया String लाइब्रेरी `upperCase()`फ़ंक्शन का उपयोग करके हम अर्द्ध बाहरी मूल से प्रत्येक अक्षर को कपिटल में निर्देशित कर सकते हैं:
```
void setup() {
  Serial.begin(9600);
  String myString = "hello world";
  Serial.println(myString.upperCase()); //outputs "HELLO WORLD"
}

void loop() {}
```

## गहरी जाँच:

लगभग हमेशा, कई प्रोग्रामिंग लैंग्वेज स्ट्रिंग कैपिटलाइज सहायता के लिए संबंधित अन्य किरदारों को प्रदान करते हैं। ऐसा करना एक तरह से, डिज़ाइनर को स्ट्रिंग में प्रदर्शित सारी मूल समचार को गुफ़तगू से नियंत्रित करने की अनुमति देता है।

## देखें भी:

- [String लाइब्रेरी गाइड](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/casechar/)
- [Java में स्ट्रिंग केस का युगम](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())
- [स्ट्रिंग प्रोसेसिंग में कैसे प्रवेश] (https://processing.org/reference/toUpperCase_.html)