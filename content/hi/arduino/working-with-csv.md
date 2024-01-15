---
title:                "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
html_title:           "Arduino: कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# हमारा आज का विषय: Arduino में CSV प्रोग्रामिंग

## Why
क्या आपने कभी सोचा हैं कि हम अपने आर्डुइनो से CSV फाइल को कैसे प्रोसेस कर सकते हैं? इससे हमें कहीं भी अपने डेटा को संग्रहीत करने और उसको एक स्थान से दूसरे स्थान पर ले जाने की सुविधा मिलती हैं। यह आपको दूसरे प्रोग्रामों से डेटा का आसानी से उपयोग भी करने देता हैं।

## How To
सबसे पहले हमें CSV पार्सिंग लाइब्रेरी को इनस्टॉल करना होगा। यह हमें CSV फाइल को प्रोसेस करने में मदद करेगा।

```arduino
#include <ArduinoCSV.h> // CSV पार्सिंग लाइब्रेरी को इन्स्टॉल करें
CSVReader reader; // फाइल से डेटा लोड करने के लिए CSVReader इंस्टेन्स बनाएं
while(reader.next()) { // हर रो पर लूप चलाएं
  String data1 = reader[0]; // पहला स्ट्रिंग कॉलम को भरें
  int data2 = int(reader[1]); // दूसरे इंटेजर कॉलम को भरें
  String data3 = reader[2]; // तीसरा स्ट्रिंग कॉलम को भरें
  // अपने स्केच में डेटा को प्रोसेस करें
}
```

### Output
इसके बाद हमें स्केच को अपने आर्डुइनो बोर्ड पर अपलोड करना होगा और सीएसवी फ़ाइल को सीरियल मॉनिटर में देखने के लिए लॉड करना होगा। हमें नंबर और टीम के नाम दोनों लिखने की आवश्यकता होगी।

```csv
1, Team A, John
2, Team B, Sarah
3, Team A, David
```

हमारे स्केच की आउटपुट कुछ इस तरह से होगी:

```
Number: 1
Team: Team A
Name: John
Number: 2
Team: Team B
Name: Sarah
Number: 3
Team: Team A
Name: David
```

## Deep Dive
CSV (Comma Separated Values) एक ऐसा फाइल फॉर्मेट हैं जो डेटा को सं