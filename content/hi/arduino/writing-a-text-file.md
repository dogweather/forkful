---
title:                "एक पाठ फाइल लिखना"
html_title:           "Arduino: एक पाठ फाइल लिखना"
simple_title:         "एक पाठ फाइल लिखना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## हम क्यों लिखें?

एक टेक्स्ट फाइल लिखना क्यों जरूरी है? बस एक शब्द में - गणना. इससे हमें अपने आर्डुइनो को उसी विधि से काम करने में आसानी होगी जैसा हम चाहते हैं।

## कैसे करें? 

आर्डुइनो में एक टेक्स्ट फाइल लिखना बहुत ही आसान है। सबसे पहले, हमें एक सेलेक्टेड सीरियल मॉनिटर के साथ "अपना" कॉम पोर्ट चुनना होगा। फिर हम एक टेक्स्ट फाइल बनाएंगे जिसे हम उसी पोर्ट के माध्यम से लिखेंगे।

```Arduino
Serial.begin(9600);
File textFile = SD.open("example.txt", FILE_WRITE);
if (textFile) {
  textFile.println("यह एक टेस्ट फाइल है।");
  textFile.close();
  Serial.println("टेक्स्ट फाइल सफलतापूर्वक लिखी गई।");
} else {
  Serial.println("टेक्स्ट फाइल लिखने में त्रुटि।");
}
```

इसके बाद हम सीरियल मॉनिटर पर संदेश देख सकते हैं कि फाइल कितने अक्षरों को सफलतापूर्वक लिखती है।

## गहराई का अध्ययन

टेक्स्ट फाइल लिखने का यह स्केच सिर्फ एक उदाहरण है। यदि आपको अधिक गहराई की जानकारी चाहिए, तो आप हमारे [आर्डुइनो की संसाधन पृष्ठ](https://www.arduino.cc/en/Tutorial/LibraryExamples/Write) पर जाकर अध्ययन कर सकते हैं। आप इससे अधिक तकनीकी जानकारी, उदाहरण कोड और स्केच और अन्य उपयोगी लिंक पा सकते हैं।

## देखें भी

- [आर्डुइनो सीरियल मॉनिटर](https://www.arduino.cc/en/serial/Serial)
- [आर्डुइनो सूचना संग्रहालय (SD)](https://www.arduino.cc/en/Reference/SD)