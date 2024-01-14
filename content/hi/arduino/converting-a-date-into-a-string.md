---
title:                "Arduino: एक तारीख को एक स्ट्रिंग में रूपांतरित करना"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

अर्डुइनो प्रोग्रामिंग में दिनांक को स्ट्रिंग में बदलना बहुत उपयोगी हो सकता है। यह स्ट्रिंग आपको उपकरण की तारीख को संग्रहीत करने और उससे सम्बंधित अन्य गणनाएं करने में मदद करेगी।

## कैसे करें

```
Arduino uno;
String date = "12/07/2021";
String date_string = "";

void setup(){
  Serial.begin(9600);
}

void loop(){
  convertDate();
  displayDate();
}

void convertDate(){
  int day = date.substring(0, 2).toInt();
  int month = date.substring(3, 5).toInt();
  int year = date.substring(6, 10).toInt();

  date_string = String(year) + "-" + String(month) + "-" + String(day);
}

void displayDate(){
  Serial.println("Converted date string: " + date_string);
}
```

यह कोड दिनांक को "वर्ष-महीना-दिन" का स्ट्रिंग में बदलेगा और सीरियल मॉनिटर पर प्रिंट करेगा। आप इसे अपनी आवश्यकताओं के अनुसार संशोधित कर सकते हैं।

## गहराई में जाएं

दिनांक को स्ट्रिंग में बदलने के लिए, हमने `substring()` और `toInt()` फंक्शन का उपयोग किया। `substring()` फंक्शन दिनांक के भिन्न अंशों को अलग-अलग स्ट्रिंग के रूप में प्रकाशित करता है। `toInt()` फंक्शन उन स्ट्रिंग को इंटीजर में बदलता है। हम इस तरह अपने दिनांक को स्ट्रिंग से अलग भागों में विभाजित करके उसे इंटीजर में बदलते हैं और उसे फिर वापस स्ट्रिंग में जोड़कर दिनांक को परिवर्तित करते हैं।

## देखें भी

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Converting String to Int in Arduino](https://www.arduino.cc/en/Tutorial/StringToInt)
- [Serial Communication in Arduino](https://www.arduino.cc/en/Tutorial/Serial)