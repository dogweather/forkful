---
title:                "Arduino: कम्प्यूटर प्रोग्रामिंग पर एक लेख - कमांड लाइन आर्ग्यूमेंट्स को पढ़ना"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप अर्दुइनो प्रोग्रामिंग के बारे में जानते हैं? और क्या आपको यह पता है कि कमांड लाइन आर्ग्यूमेंट्स क्या होते हैं और उनका उपयोग क्यों किया जाता है? इस ब्लॉग पोस्ट में, हम आपको बताएँगे कि अर्दुइनो में कमांड लाइन आर्ग्यूमेंट्स कैसे पढ़े जाते हैं और इसके क्या फायदे हैं।

## कैसे करें

अर्दुइनो में कमांड लाइन आर्ग्यूमेंट्स पढ़ने के लिए, हमें ```Arduino IDE``` का उपयोग करना होगा। आप स्केच या प्रोग्राम में कुछ पैरामीटर या वैल्यू को पास कर सकते हैं और उन्हें ```void setup()``` या ```void loop()``` में उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में हम आपको इसका एक उदाहरण दिखाएंगे:

```arduino
int ledPin = 13;

void setup() {
  pinMode(ledPin, OUTPUT);
  Serial.begin(9600);
  // कमांड लाइन आर्ग्यूमेंट्स को पढ़ने के लिए, हमें Serial.begin() के माध्यम से सीरियल मॉनिटर को तैयार करना होगा
}

void loop() {
  // कमांड लाइन से आर्ग्यूमेंट पढ़ना
  String input = Serial.readString(); //Serial.readString() की मदद से हम आर्ग्यूमेंट को String के रूप में पढ़ सकते हैं
  if (input == "ON") {
    digitalWrite(ledPin, HIGH);
    // जब हम Serial मॉनिटर में ON लिखेंगे, तो एलईडी जलेगा
  } else if (input == "OFF") {
    digitalWrite(ledPin, LOW);
    // जब हम Serial मॉनिटर में OFF लिखेंगे, तो एलईडी बंद हो जाएगा
  }
}
```

जैसा कि आप देख सकते हैं, हमने ```Serial.readString()``` की मदद से सीरियल मॉनिटर से लिखे गए आर्ग्यूमेंट क