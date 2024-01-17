---
title:                "स्ट्रिंग्स को मिलाना"
html_title:           "Arduino: स्ट्रिंग्स को मिलाना"
simple_title:         "स्ट्रिंग्स को मिलाना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कोई भी प्रोग्राम लिखते समय आपने शायद सबसे अधिक स्ट्रिंग (चर सूची) को संयुक्त बनाने का प्रयास किया होगा। यह स्ट्रिंग जोड़ना (concatenating strings) कहलाता है और यह प्रोग्रामर्स के लिए बहुत उपयोगी होता है।

## कैसे करें:

```
// संयुक्त स्ट्रिंग का उदाहरण
String firstname = "जॉन";
String lastname = "डो";
String full_name = firstname + " " + lastname;

// उत्पादन
जॉन डो
```

## गहराई से जानें:

पहले, स्ट्रिंग जोड़ने (concatenating) की व्यावहारिक स्थिति चर जोड़ने के रूप में शुरू हुई थी, और अब यह भी प्रोग्रामिंग के सभी क्षेत्रों में उपयोगी होता है। इसके अलावा, आप अलग-अलग स्ट्रिंग जोड़ने के लिए विभिन्न तरीकों का भी प्रयोग कर सकते हैं, जैसे की String.concat() और stringstream। इसके लिए, आपको संदर्भ (reference) ध्यान में रखना होगा कि कौन-सा तरीका किस स्थिति में सर्वोत्तम होगा।

## भी देखें:

- [अर्धनगत सुश्रुत](https://en.wikipedia.org/wiki/Concatenation)
- [अर्धनगत पाथवेया](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)