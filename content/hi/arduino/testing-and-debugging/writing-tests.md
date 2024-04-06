---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:14.337310-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Arduino \u092E\u0947\
  \u0902 \u0915\u0941\u091B \u0905\u0928\u094D\u092F \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0935\u093E\u0924\u093E\u0935\u0930\u0923\
  \u094B\u0902 \u0915\u0940 \u0924\u0930\u0939 \u090F\u0915 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u091F\u0947\u0938\u094D\u091F\u093F\u0902\u0917 \u092B\u094D\
  \u0930\u0947\u092E\u0935\u0930\u094D\u0915 \u0928\u0939\u0940\u0902 \u0939\u0948\
  \u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\u092A \u0925\u0930\u094D\
  \u0921 \u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940\u091C \u091C\u0948\u0938\u0947 \u0915\u093F `AUnit`\u2026"
lastmod: '2024-03-13T22:44:52.779538-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u092E\u0947\u0902 \u0915\u0941\u091B \u0905\u0928\u094D\u092F \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0935\u093E\u0924\
  \u093E\u0935\u0930\u0923\u094B\u0902 \u0915\u0940 \u0924\u0930\u0939 \u090F\u0915\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u091F\u0947\u0938\u094D\u091F\u093F\
  \u0902\u0917 \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\u0915 \u0928\u0939\
  \u0940\u0902 \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0906\
  \u092A \u0925\u0930\u094D\u0921 \u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u091C\u0948\u0938\u0947 \u0915\
  \u093F `AUnit` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 Arduino\
  \ \u0915\u094B\u0921 \u0915\u0940 \u092F\u0942\u0928\u093F\u091F \u091F\u0947\u0938\
  \u094D\u091F\u093F\u0902\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902\u0964 AUnit, Arduino \u0915\u0947 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940, `ArduinoUnit`, \u0914\u0930\
  \ Google \u0915\u0947 \u091F\u0947\u0938\u094D\u091F\u093F\u0902\u0917 \u092B\u094D\
  \u0930\u0947\u092E\u0935\u0930\u094D\u0915, `Google Test` \u0938\u0947 \u092A\u094D\
  \u0930\u0947\u0930\u093F\u0924 \u0939\u0948\u0964\n"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे करें:
Arduino में कुछ अन्य प्रोग्रामिंग वातावरणों की तरह एक बिल्ट-इन टेस्टिंग फ्रेमवर्क नहीं है। हालांकि, आप थर्ड पार्टी लाइब्रेरीज जैसे कि `AUnit` का उपयोग कर Arduino कोड की यूनिट टेस्टिंग कर सकते हैं। AUnit, Arduino के बिल्ट-इन लाइब्रेरी, `ArduinoUnit`, और Google के टेस्टिंग फ्रेमवर्क, `Google Test` से प्रेरित है।

### AUnit के साथ उदाहरण:
सबसे पहले, Arduino IDE में Library Manager के माध्यम से AUnit इंस्टॉल करें: Sketch > Include Library > Manage Libraries... > AUnit के लिए खोज करें और इसे इंस्टॉल करें।

फिर, आप इस तरह से परीक्षण लिख सकते हैं:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // खाली
}
```
इस परीक्षण को अपने Arduino बोर्ड पर अपलोड करने के बाद, परीक्षण परिणाम देखने के लिए Serial Monitor खोलें। आपको परिणाम देखने को मिलेंगे जिससे पता चलेगा कि प्रत्येक परीक्षण पास हुआ या विफल हुआ:

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

यह सरल उदाहरण AUnit का उपयोग कर एक LED पिन की स्थिति का परीक्षण करने का दर्शाता है। परीक्षण बनाकर, आप पुष्टि करते हैं कि आपका Arduino विभिन्न स्थितियों में अपेक्षित रूप से व्यवहार करता है। AUnit के साथ, आप अधिक जटिल परीक्षण, परीक्षण सूट्स लिख सकते हैं, और अधिक उन्नत परिदृश्यों के लिए जैसे कि परीक्षण टाइमआउट और सेटअप/टियरडाउन प्रक्रियाएं का आनंद ले सकते हैं।
