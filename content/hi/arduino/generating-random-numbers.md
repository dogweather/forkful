---
title:                "Arduino: कम्प्यूटर प्रोग्रामिंग पर लेख: रैंडम नंबरों का उत्पादन"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आपको बिना किसी वजह के या उद्देश्य के नंबर गेनरेट करने के लिए आर्डुइनो प्रोग्रामिंग का उपयोग क्यों करना चाहिए? शायद आपको नहीं लगता हो, लेकिन बहुत से प्रोजेक्ट्स में ये फ़ंक्शन बहुत उपयोगी होता है! नंबर जेनरेट करने की क्षमता आपको अनेक अनुप्रयोगों में सहायता प्रदान कर सकती है, जैसे कि गेम डेवलपमेंट, डाटा एनक्रिप्शन, क्रिप्टोकरेंसी जैसे क्षेत्रों में। अब चलिए देखते हैं कि आप इसे कैसे कर सकते हैं!

## कैसे

```Arduino
void setup() {
  Serial.begin(9600); // Initialize serial monitor
  randomSeed(analogRead(A0)); // Use analog pin A0 for random seed
}

void loop() {
  int randomNumber = random(0,10); // Generate random number between 0 and 9
  Serial.println(randomNumber); // Print random number on serial monitor
  delay(1000); // Delay for 1 second
}
```

ी आपको दिखाए गए कोड से आप देख सकते हैं कि `random()` फ़ंक्शन को कैसे यूज़ किया जाता है। पहले हम `setup()` फ़ंक्शन में `Serial` और `randomSeed()` को इनिशलाइज़ करते हैं। यह सेटिंग्स हर बार `loop()` फ़ंक्शन के बारे में चलते हैं। फिर आप `random()` फ़ंक्शन के माध्यम से चाहे जो रैंडम नंबर जेनरेट कर सकते हैं और `Serial.println()` से उसे सीरियल मॉनिटर पर प्रिंट भी कर सकते हैं। अब आप जो भी सीरियल मॉनिटर पर रैंडम नंबर देखेंगे वो `delay()` फ़ंक्शन के माध्यम से एक सेकंड के लिए रोकेंगे। यह दोहराया जाएगा जब तक आप अपने आर्डुइनो बोर्ड को रणनीतिक सिरफ़ नोट दिया जाता हो