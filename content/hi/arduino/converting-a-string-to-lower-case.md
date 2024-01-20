---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को निचले मामले में बदलना, इसका मतलब है कि सभी वर्णमाला को छोटे में कनवर्ट करना। प्रोग्रामर इसे डाटा संगतता, तुलना, और खोज को सरल बनाने के लिए करते हैं।

## कैसे करें:
आर्डुइनो (वर्तमान संस्करण) के साथ, आप `toLowerCase()` विधि का उपयोग करके इसे कर सकते हैं। 

```Arduino
String myString = "HELLO, WORLD!";
myString.toLowerCase();
Serial.println(myString);  // Prints: hello, world!
```

## गहरा डाइव
1. ऐतिहासिक प्रसंग: निम्नलिखित मामले में स्ट्रिंग कनवर्शन की आवश्यकता डाटा माइनिंग, खोज गणनाएँ, और यांत्रिक भाषा संसाधन के प्रारंभ से ही रही है।

2. विकल्प: आर्डुइनो भाषा के अलावा, कुछ भी सोचने की आवश्यकता नहीं है। `toLowerCase()` किसी भी स्ट्रिंग पर ऑपरेट करने के लिए सबसे आसान, सरल और शुद्ध तरीका है। 

3. कार्यान्वयन विवरण: `toLowerCase()` विधि अपने समर्थित कक्ष को निम्नवर्ण के रूप में बदलती है, जिससे पूरे स्ट्रिंग पर एक सरल अक्षर-द्वारा परिवर्तन संभव होता है। 

## और देखें
1. आर्डुइनो String `toLowerCase()` विधि: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/
2. Arduino संदर्भ गाइड: https://www.arduino.cc/reference/en/
3. स्ट्रिंग कण्वर्शन के बारे में अधिक जानने के लिए: https://www.w3schools.com/jsref/jsref_tolowercase.asp