---
title:                "टेस्ट लिखना"
date:                  2024-02-03T19:30:14.337310-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Arduino वातावरण में परीक्षण लिखना, आटोमेटेड परीक्षणों की रचना की प्रक्रिया को संदर्भित करता है जो Arduino उपकरणों पर आपके कोड की कार्यक्षमता को मान्य करते हैं। प्रोग्रामर इसे सुनिश्चित करने के लिए करते हैं कि उनका कोड अपेक्षित रूप से कार्य करे, बग्स को कम करे, और उनकी परियोजनाओं की गुणवत्ता में सुधार करे, विशेष रूप से एंबेडेड सिस्टम्स में जहाँ डिबगिंग अधिक चुनौतीपूर्ण हो सकती है।

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
