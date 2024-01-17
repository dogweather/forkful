---
title:                "स्ट्रिंग की अंतर्वर्तना"
html_title:           "Arduino: स्ट्रिंग की अंतर्वर्तना"
simple_title:         "स्ट्रिंग की अंतर्वर्तना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अपने एप्लिकेशन के साथ डेटा को सुव्यवस्थित रूप से प्रदर्शित करने के लिए आम तौर पर हम स्ट्रिंग को इंटरपोलेट करते हैं। यह डेटा को एक दूसरे से अलग करने और उसे स्पष्ट और समझने में आसानी बनाता है। प्रोग्रामर्स इसे अपनी कोड को अधिक दृश्यमान और संगठित बनाने के लिए करते हैं।

## कैसे करें:
```Arduino
int number = 10;
String message = "The number is: ";
Serial.println(message + number);
```
यह प्राथमिक उदाहरण एक स्ट्रिंग को नंबर के साथ इंटरपोलेट करता है। आप सरल उदाहरणों के साथ दूसरे वेरिएंट भी प्रयोग कर सकते हैं। आगे नहीं बढ़ते हैं।

## गहराई में जाएं:
स्ट्रिंग की इंटरपोलेशन का उपयोग प्राथमिक रूप से खोजने में आसान और सादा होता था। अब इसे अलग-अलग प्लेटफार्म्स पर समर्थित मुख्य संग्रह करता है। विकल्प हैं, जैसे फ़ॉर्मेट, जो इसके एक दूसरे प्रशंसक हैं। स्ट्रिंग के अंदर की व्यवस्थापन के लिए बेहतरीन समर्थन के लिए स्ट्रिंग को एक शेमा के साथ बांधा गया है।

## और भी देखिये:
अधिक जानकारी के लिए, आप अर्डुइनो दस्तावेज़ीकरण पढ़ सकते हैं और संबंधित स्रोतों को निम्नलिखित लिंक के माध्यम से जांच सकते हैं: 
- https://www.arduino.cc/reference/tr/language/variables/data-types/string/ 
- https://www.arduino.cc/reference/tr/language/functions/string-functions/ 
- https://beginnersbook.com/2017/08/c-string-interpolation/