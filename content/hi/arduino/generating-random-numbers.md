---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:26.566477-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स वह होते हैं जो किसी पैटर्न या नियमितता के बिना जनरेट होते हैं। प्रोग्रामर्स गेमिंग, सिमुलेशन, और सिक्योरिटी में भिन्नता और पूर्वानुमान न कर पाने वाले परिणामों के लिए इनका उपयोग करते हैं।

## How to: (कैसे करें:)
Arduino में `random()` फ़ंक्शन का इस्तेमाल करके रैंडम नंबर्स जनरेट किए जा सकते हैं। नीचे एक उदाहरण है:

```arduino
void setup() {
  Serial.begin(9600);  // सीरियल मॉनिटर शुरू करते हैं
  randomSeed(analogRead(0));  // रैंडम सीड शुरू करने के लिए एक अप्रत्याशित इनपुट
}

void loop() {
  int randomNumber = random(1, 100);  // 1 से 100 के बीच एक रैंडम नंबर जेनरेट करता है
  Serial.println(randomNumber);  // सीरियल मॉनिटर पर रैंडम नंबर डिस्प्ले करता है
  delay(1000);  // एक सेकंड के लिए रुक जाता है
}
```
इस प्रोग्राम को चलाने पर आप हर सेकंड 1 से 100 के बीच एक नई रैंडम संख्या देखेंगे।

## Deep Dive (गहराई से जानकारी)
`randomSeed()` फ़ंक्शन से रैंडम नंबर जनरेटर को एक शुरुआती बिन्दु (सीड) मिलता है, जिसकी मदद से अगले रैंडम नंबर्स अनियमित लगते हैं। अगर आप सीड नहीं देंगे, तो हर बार सेम नंबर सीरीज जनरेट होगी। एनालॉग पिन से अनिश्चित वोल्टेज पढ़कर, जैसे `analogRead(0)`, अच्छा सीड प्राप्त होता है।

तकनीकी रूप से, Arduino में रैंडम नंबर्स असल में "प्स्यूडो-रैंडम" होते हैं क्योंकि वे एक निश्चित एल्गोरिदम का पालन करते हैं। वास्तविक रैंडमनेस बहुत मुश्किल से ही प्राप्त होती है, पर अधिकांश एप्लीकेशंस के लिए प्स्यूडो-रैंडम नंबर्स काफ़ी होते हैं।

## See Also (और भी देखें)
- Arduino का `randomSeed()` रेफरेंस: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- Arduino का `random()` रेफरेंस: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- माइक्रोकंट्रोलर्स में रैंडम नंबर जनरेशन के सिद्धांत: http://www.schneier.com/paper-pseudorandom-sequence.html
