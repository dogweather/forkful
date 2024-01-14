---
title:    "Arduino: प्रोग्रामिंग में टेस्ट लिखना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों

टेस्ट लिखने के माध्यम से आप अपने अर्डुइनो प्रोग्राम को सुरक्षित रख सकते हैं और प्रोग्राम के सही काम करने की गारंटी दे सकते हैं। इसके अलावा, टेस्ट कोड का उपयोग करके आप अपने प्रोग्राम को धीरे-धीरे और आसानी से बदल सकते हैं।

## कैसे

```Arduino
void setup(){
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

ऊपर कोड में, हमने एक साधारण एलईडी ब्लिंक कार्यक्रम दिखाया है। लेकिन हमने टेस्ट भी लिखा है जो यह सुनिश्चित करता है कि एलईडी ब्लिंक कार्यक्रम ठीक से काम कर रहा है।

```Arduino
void test_LED_Blink(){
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  bool value = digitalRead(LED_BUILTIN);
  assertEqual(value, HIGH);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
  bool new_value = digitalRead(LED_BUILTIN);
  assertEqual(new_value,LOW);
}
```

विस्तार से देखें, हमने सबसे पहले `LED_BUILTIN` को शुरू में चालू किया है। फिर हमने `digitalRead()` का उपयोग करके `LED_BUILTIN` के स्थिति को जांचा है। हम फिर से एलईडी को बंद करते हैं और फिर से उसकी स्थिति को जांचते हैं। आप इस टेस्ट को चलाकर देख सकते हैं कि अर्डुइनो कण्ठ का सही स्थिति कोड को काम करने के बाद दो बार ही बदलता है।

## गहराई तक

टेस्ट लिखने के लिए आपको कुछ रोधियां हो सकती हैं, लेकिन यह आपके अर्डुइनो प्रोग्राम को सुरक्षित बनाने के लिए आवश्यक है। आप अपने प्रोग्राम को बहुत साफ़ और अच्छी तरह से संगठित कर सकते हैं, जिससे आपको उसे बदलने म