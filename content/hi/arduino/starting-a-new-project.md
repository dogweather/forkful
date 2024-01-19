---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
लगभग हर नया सॉफ़्टवेयर प्रोजेक्ट एक नई बाजारी पहचान के साथ शुरू होता है, जिससे प्रोग्रामर्स को अपना आविष्कार बेहतर तरीके से मार्केट करने में सहायता मिलती है। प्रोग्रामर्स इसे नए समस्याओं को हल करने, अपने क्षमताओं को तेज करने और नवीनतम तकनीक को सीखने के लिए करते हैं।

## कैसे करें:
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
इस सादे कोड का परिणाम एलईडी को हर सेकंड के बाद अपने हाई और लो स्थितियों में बदल कर चमकाना है।

## गहरा डाइव:
एक नया प्रोजेक्ट शुरू करना Arduino के साथ कोडिंग की दुनिया में पहले कदम होता है। Arduino ने 2005 में इसे खुला स्रोत किया ताकि कोई भी इस पर काम कर सके। विकल्प के रूप में आप RPi, PIC, Atmel आदि प्लेटफार्म पर भी काम कर सकते हैं। इन प्लेटफार्मों ने अलग-अलग छुपी हुई बातों को सामने लाने और नए प्रोजेक्ट की शुरुआत करने के लिए अपने आपको तैयार किया है।

## देखें भी:
- [Arduino Home](https://www.arduino.cc/)
- [Arduino Programming Guide](https://www.arduino.cc/en/Guide/HomePage)
- [Arduino Programming Language Reference](https://www.arduino.cc/reference/en/)
- [Arduino Project Hub](https://create.arduino.cc/projecthub)

इस जानकारी की सहायता से आप अर्दुइनो के साथ प्रोजेक्ट शुरू करने में सहायता पा सकते हैं। यदि आपको और मदद चाहिए, तो ऊपर दिए गए लिंक्स देखें।