---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-27T20:33:39.760933-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Arduino प्रोजेक्ट्स में यादृच्छिक संख्याएं उत्पन्न करना अनुमान लगाने योग्य नहीं होने वाले मानों का निर्माण करने की प्रक्रिया है, जो कि खेलों, सिमुलेशनों, और सुरक्षा प्रणालियों जैसे अनुप्रयोगों के लिए महत्वपूर्ण है। प्रोग्रामर्स इस तकनीक का उपयोग विविधता पेश करने या ऐसे निर्णय लेने के लिए करते हैं जो न्यूनतापूर्वक नहीं होने चाहिए।

## कैसे:
Arduino यादृच्छिक संख्याओं को उत्पन्न करने के लिए सीधे-सादे फ़ंक्शन प्रदान करता है: `randomSeed()` और `random()`. शुरू करने के लिए, यादृच्छिक संख्या जेनरेटर को सीड करें ताकि हर बार आपका प्रोग्राम चल रहा हो तो विभिन्न क्रमों की संख्या सुनिश्चित हो। एक अक्सर उपयोग की जाने वाली विधि एक अनजुड़े पिन से एनालॉग पढ़ाई के साथ सीडिंग करना है।

```Arduino
void setup() {
  Serial.begin(9600);
  // यादृच्छिक सीड को प्रारंभ करें
  randomSeed(analogRead(0));
}

void loop() {
  // 0 और 99 के बीच एक यादृच्छिक संख्या उत्पन्न करें
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // पढ़ाई की सुगमता के लिए एक सेकंड की देरी
}
```

उपरोक्त प्रोग्राम `setup()` फ़ंक्शन में यादृच्छिक संख्या जेनरेटर को प्रारंभ करता है और प्रत्येक लूप इटरेशन में 0 और 99 के बीच एक नई संख्या उत्पन्न करता है, और संख्या को सीरियल मॉनिटर पर आउटपुट करता है।

नमूना आउटपुट:
```
42
17
93
...
```

## गहन जानकारी
Arduino का `random()` फ़ंक्शन आंतरिक रूप से एक प्रायोज्य-यादृच्छिक संख्या जेनरेटर (PRNG) का उपयोग करता है, जो एक न्यूनतापूर्वक अनुक्रम का अनुसरण करता है लेकिन सांख्यिकीय रूप से यादृच्छिक प्रतीत होता है। अनुक्रम का प्रारंभिक मूल्य, या सीड, इसकी अनिश्चितता पर गहरा प्रभाव डालता है, इसलिए `randomSeed()` का उपयोग किसी हद तक यादृच्छिक इनपुट के साथ एक प्रारंभिक बिंदु के रूप में करना सामान्य है। यह महत्वपूर्ण है कि Arduino द्वारा उत्पन्न रैंडमनेस अधिकतर हॉबीस्ट प्रोजेक्ट्स के लिए पर्याप्त है लेकिन अपनी पूर्वानुमेयता के कारण उच्च-सुरक्षा अप्लिकेशनों के मानदंडों को पूरा नहीं कर सकता है। क्रिप्टोग्राफिक उद्‌देश्यों के लिए, अधिक सोफिस्टिकेटेड एल्गोरिद्मों और हार्डवेयर यादृच्छिक संख्या जेनरेटरों (HRNGs) की ओर देखना सलाह दी जाती है, जो भौतिक प्रक्रियाओं का उपयोग करके सच्ची यादृच्छिकता प्रदान कर सकते हैं।