---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्गुमेंट्स पढ़ना का मतलब है कोड संचालित करने के समय प्रदान की गई विवरण पढ़ना। प्रोग्रामर इसे इसलिए करते हैं, ताकि उन्हें चलाने के समय अन्य मानों का निर्धारण करने की आवश्यकता न पड़े।

## कैसे

इंपलिमेंटेशन के लिए एक सरल उदाहरण प्रस्तुत कर रहे हैं। 

```
// एड्रेस पिन डेफ़ाइन करें
#define ADDR_PIN0 2
#define ADDR_PIN1 3

void setup() {
  // सीरियल पोर्ट खोलना
  Serial.begin(9600);
  
  // पिनों को आउटपुट के रूप में सेट करें
  pinMode(ADDR_PIN0, OUTPUT);
  pinMode(ADDR_PIN1, OUTPUT);
}

// कार्य का उद्धरण
void loop() {
  
  // इसका उपयोग डिजिटल वैल्यू को HIGH या LOW सेट करने के लिए करें
  digitalWrite(ADDR_PIN0, HIGH); 
  digitalWrite(ADDR_PIN1, LOW);

  delay(2000); // 2 seconds delay  
}
```
## गहरी डाइव 

1. **ऐतिहासिक संदर्भ:** Arduino एक ओपन सोर्स हार्डवेयर और सॉफ़्टवेयर प्लेटफ़ॉर्म है जिसने माइक्रोकंट्रोलर प्रोग्रामिंग को सरल बनाया। 

2. **विकल्प:** अधिक जटिल और विशाल प्रोजेक्ट्स के लिए, आप अन्य माइक्रोकंट्रोलर बोर्ड जैसे कि Raspberry Pi का उपयोग कर सकते हैं। 

3. **कार्यान्वयन विवरण:** कमांड लाइन आर्गुमेंट का उपयोग केवल Arduino एप्लिकेशन को कस्टमाइज़ करने के लिए है, जबकि उन्हें बाद में पढ़ना या मोडिफ़ाई करना नहीं है।

## देखें भी 

1. [Arduino कमांड लाइन टूल](https://www.arduino.cc/en/Main/CommandLine)
2. [Arduino बोर्ड के लिए रास्पबेरी पाई](https://www.raspberrypi.org/)
3. [Arduino फोरम](https://forum.arduino.cc/index.php)