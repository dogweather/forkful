---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:04.161359-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग की लम्बाई पता करना सिर्फ यह ज्ञात करना है कि उसमें कितने अक्षर हैं। प्रोग्रामर इसलिए लम्बाई पता करते हैं ताकि कोड में स्ट्रिंग्स को संभालना और उन पर ऑपरेशन्स करना आसान हो जाए।

## कैसे करें:
Arduino कोड में स्ट्रिंग की लम्बाई जानना सरल है. `length()` फंक्शन का उपयोग करके आप ऐसा कर सकते हैं:

```arduino
void setup() {
  // सीरियल कम्यूनिकेशन शुरू करें
  Serial.begin(9600);
  
  // स्ट्रिंग बनाएं
  String myString = "ArduinoProgramming";
  
  // लम्बाई निकालें और प्रिंट करें
  unsigned int lengthOfString = myString.length();
  Serial.print("String Length: ");
  Serial.println(lengthOfString);
}

void loop() {
  // कुछ नहीं करना है यहाँ
}
```

सैंपल आउटपुट होगा:
```
String Length: 18
```

## गहराई में:
स्ट्रिंग की लम्बाई पता करने का तरीका अरुइनो प्रोग्रामिंग का एक बुनियादी हिस्सा है। पहले के समय में, C जैसी प्रोग्रामिंग लैंग्वेज में, इसे पता करने के लिए `strlen()` फंक्शन का इस्तेमाल होता था, जो कि नल-टर्मिनेटेड स्ट्रिंग्स के लिए काम करता है। Arduino में `length()` फंक्शन उससे ज्यादा सीधा और सुविधाजनक है।

यह भी जरूरी है कि हम जब लम्बाई नापें तो परफॉर्मेंस का भी ख्याल रखें क्योंकि हर बार `length()` कॉल करना CPU टाइम बर्बाद कर सकता है अगर लम्बाई बार-बार नहीं बदल रही हो तो।

## और देखें:
स्ट्रिंग्स और उनके ऑपरेशन्स को बेहतर समझने के लिए निम्न लिंक्स की जांच करें:

- Arduino Official String Reference: [Arduino - StringObject](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- C++ `std::string` Reference: [std::string - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string)
