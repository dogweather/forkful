---
title:                "Arduino: स्ट्रिंगों को संयोजित करना"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों
आपने अभी तक Arduino के कोडिंग में string concatenation शब्द सुने ही होंगे। इसका मतलब है कि आप कुछ अलग-अलग strings को एक साथ जोड़ना चाहते हैं। यह आमतौर पर वर्कशीट को अभिव्यक्त करने के लिए किया जाता है, जब आपको विशेष तारीके से संग्रहीत करना होता है।

## कैसे
```Arduino
String str1 = "Hello";
String str2 = "world";
Serial.println(str1 + " " + str2); /*output: Hello world*/
```

आप देख सकते हैं कि यहां हमने दो strings को प्रिंट करने के लिए "+" ऑपरेटर का प्रयोग किया है। साथ ही हमने पढ़ा कि Arduino में strings को concatenate करने के लिए "+","+=" और "concat()" ऑपरेटर उपलब्ध है। आप जितनी चाहें उतने स्ट्रिंग्स को जोड़ सकते हैं।

## गहराई में
जब आप strings को "+" ऑपरेटर से जोड़ते हैं, तो यह वास्तव में उस strings को concatenate करता है जो उस वक्त में पहले से ही मौजूद होते हैं। अगर आप उस strings को इसके बाद बदलते हैं, तो यह उसको स्वतः आगे चलकर concatenate करता है। और अगर आप इस strings को किसी भी प्रकार से modify करते हैं, तो आपको इसे सामान्य सुम्मा की तरह ही concatenate करना होगा।

## देखें भी
- [String concatenation tutorial by Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [String concatenation examples by Hackster](https://www.hackster.io/Arduino_Genuino/string-concatenation-using-arduino-9fc181)
- [Tutorial on using the String library by GeeksforGeeks](https://www.geeksforgeeks.org/working-with-strings-using-string-library-in-arduino-ide/)