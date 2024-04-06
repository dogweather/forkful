---
date: 2024-01-20 17:42:00.907489-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: (How to) Arduino\
  \ \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938\
  \ \u0938\u0947 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u0948\u091A \u0915\u0930\
  \u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\
  \u0930\u094D\u0938 \u0915\u094B \u0939\u091F\u093E\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0941\u091B \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\
  \ \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0928\u0939\u0940\u0902 \u0939\
  \u094B\u0924\u0947, \u0924\u094B \u0939\u092E\u0947\u0902 \u092E\u0948\u0928\u094D\
  \u092F\u0941\u0905\u0932\u0940 \u0910\u0938\u093E \u0915\u0930\u0928\u093E\u2026"
lastmod: '2024-04-05T21:53:54.710749-06:00'
model: gpt-4-1106-preview
summary: "(How to) Arduino \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917\u094D\u0938 \u0938\u0947 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\
  \u0948\u091A \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\
  \u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B \u0939\u091F\u093E\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0941\u091B \u0921\u093E\u092F\u0930\
  \u0947\u0915\u094D\u091F \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0928\
  \u0939\u0940\u0902 \u0939\u094B\u0924\u0947, \u0924\u094B \u0939\u092E\u0947\u0902\
  \ \u092E\u0948\u0928\u094D\u092F\u0941\u0905\u0932\u0940 \u0910\u0938\u093E \u0915\
  \u0930\u0928\u093E \u092A\u0921\u093C\u0924\u093E \u0939\u0948\u0964 \u0928\u0940\
  \u091A\u0947 \u090F\u0915 \u0938\u093F\u0902\u092A\u0932 \u0915\u094B\u0921 \u0926\
  \u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## कैसे करें: (How to)
Arduino में स्ट्रिंग्स से पैटर्न मैच करने वाले कैरेक्टर्स को हटाने के लिए कुछ डायरेक्ट फंक्शन्स नहीं होते, तो हमें मैन्युअली ऐसा करना पड़ता है। नीचे एक सिंपल कोड दिया गया है:

```Arduino
void setup() {
  Serial.begin(9600);
  String data = "Hello123World456";
  String pattern = "0123456789";
  data = deletePattern(data, pattern);
  Serial.println(data);
}

void loop() {
  //Nothing to do here
}

String deletePattern(String str, String pattern) {
  for (int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```

जब ऊपर वाला कोड Arduino पर चलेगा, तो सीरियल मॉनिटर पर यह दिखेगा:

```
HelloWorld
```

## गहराई में: (Deep Dive)
Arduino में स्ट्रिंग्स से किसी पैटर्न को हटाने का काम आमतौर पर `replace()` फंक्शन से किया जाता है, जैसा कि हमने हमारे उदाहरण में किया है। चूंकि MCU (Microcontroller Units) जैसे Arduino में सीमित मेमोरी और प्रोसेसिंग पावर होती है, यहाँ डेटा मैनिपुलेशन का काम बहुत ही सावधानी से करना पड़ता है। इतिहास में, Text editing और Pattern matching (regex) के लिए माइक्रोकंट्रोलर्स बहुत कम इस्तेमाल होते थे, लेकिन आज एम्बेडेड सिस्टम्स में स्मार्ट प्रोसेसिंग की बढ़ती जरूरत के कारण ये ऑपरेशन्स ज्यादा सामान्य होते जा रहे हैं। 

कुछ मामलों में, प्रोग्रामर्स ने अपने खुद के फंक्शन्स का निर्माण किया होता है जो एफिशिएंट और सिस्टम के रिसोर्स के अनुसार होते हैं।

## यह भी देखें: (See Also)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Official Forum](https://forum.arduino.cc/)
- [Arduino Regex Library](https://github.com/nickgammon/Regexp) - Pattern matching with regular expressions in Arduino sketches.
