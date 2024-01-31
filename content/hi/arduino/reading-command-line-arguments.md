---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:09.227049-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Arduino में कमांड लाइन आर्ग्यूमेंट्स पढ़ने की बात थोड़ी अलग होती है क्योंकि इसमें पारंपरिक कमांड लाइन इंटरफ़ेस नहीं होता। किन्तु, सीरियल कमांड्स को पढ़ना और उनका प्रयोग करना यही कार्य करता है। हम इसे प्रोग्राम करते हैं ताकि बाहरी इनपुट्स के आधार पर हमारे Arduino प्रोजेक्ट्स इंटरएक्टिव हो सकें।

## How to: (कैसे करें?)

```Arduino
void setup() {
  Serial.begin(9600);  // सीरियल कम्युनिकेशन शुरू करें
}

void loop() {
  if (Serial.available() > 0) {  // जांचें कि क्या कुछ सीरियल इनपुट उपलब्ध है
    char c = Serial.read();  // कैरेक्टर पढ़ें
    Serial.print("आपने प्रविष्ट किया: ");
    Serial.println(c);  // पढ़ा हुआ कैरेक्टर प्रदर्शित करें
  }
}
```

Sample Output:
```
आपने प्रविष्ट किया: a
आपने प्रविष्ट किया: b
```

## Deep Dive (गहराई में जानकारी)

ऐतिहासिक रूप से देखें, तो Arduino बोर्ड्स का इस्तेमाल सरल माइक्रोकंट्रोलर प्रोजेक्ट्स के लिए हुआ करता था। समय के साथ, लोगों ने इसे सीरियल कम्युनिकेशन के माध्यम से बाहरी संसार से इंटरएक्ट करने के लिए प्रयोग किया। जबकि Arduino आइडीई ट्रू कमांड लाइन आर्ग्यूमेंट्स का सहारा नहीं लेता, Serial पुस्तकालय कа प्रयोग करना इसी काम को संभव बनाता है। अल्टरनेटिव के तौर पर, आप अन्य कम्युनिकेशन प्रोटोकॉल्स जैसे I2C या SPI का भी उपयोग कर सकते हैं।

## See Also (और भी देखें)

- Arduino Serial पुस्तकालय का दस्तावेज (https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- I2C प्रोटोकॉल पर एक ट्यूटोरियल (https://www.arduino.cc/en/Tutorial/MasterReader)
- SPI कम्युनिकेशन के बारे में जानकारी (https://www.arduino.cc/en/Reference/SPI)
