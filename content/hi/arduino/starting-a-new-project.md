---
title:                "नवीन प्रोजेक्ट शुरू करना"
html_title:           "Arduino: नवीन प्रोजेक्ट शुरू करना"
simple_title:         "नवीन प्रोजेक्ट शुरू करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# "## क्या और क्यूँ?"
नई परियोजना शुरू करना क्या है, और क्यों प्रोग्रामर्स इसे करते हैं? प्रोग्रामर्स नए परियोजनाओं को आरंभ करके नए सॉफ्टवेयर, हार्डवेयर या आपलोडेड कोड को बनाने के लिए शुरू करते हैं।

# "## कैसे करें:"
Arduino को अपने कंप्यूटर से डाउनलोड करें और उसे इंस्टॉल करें। यदि आपके पास पहले से ही Arduino नई परियोजना है, तो आप इसे खोलने के लिए उसे चलाएं। यदि नहीं, तो आप एक नई परियोजना बनाएं और कोड को आरंभ करने के लिए अपने Arduino बोर्ड को कंप्यूटर से जोड़ें। नीचे दिए गए कोड को कॉपी और पेस्ट करके आप अपने परियोजना को आरंभ कर सकते हैं।

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

# "## गहराई में जाएँ:"
क्या आप जानते हैं कि Arduino को Massimo Banzi और David Cuartielles द्वारा पहली बार 2005 में बनाया गया था? इसके अलावा, आप अपने परियोजना में Arduino के साथ कनेक्ट होने के लिए कई अन्य माइक्रोकंट्रोलर भी उपयोग कर सकते हैं। आप अपने कोड में नए कार्यकारी या स्लाइडर सेंसर को जोड़कर अपने परियोजना को और भी रोचक बना सकते हैं।

# "## और भी देखें:"
आप अपनी परियोजना को आरंभ करने से पहले और अधिक सेंसर और विभिन्न आर्थिक कार्यकारी कोड स्केच को ढूंढने के लिए निम्न लिंकों पर जा सकते हैं: 
- https://www.arduino.cc/en/Guide/HomePage
- https://www.arduino.cc/reference/en/
- https://www.arduino.cc/getting-started/examples/