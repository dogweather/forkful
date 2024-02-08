---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:57:27.282875-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

टेक्स्ट सर्च और रिप्लेस करना मतलब एक शब्द या वाक्य को खोजकर उसे नए शब्द या वाक्य से बदलना। प्रोग्रामर्स यह कार्य डाटा को सही करने, गलतियों को ठीक करने, और कोड को अपडेट करने के लिए करते हैं।

## कैसे करें? (How to:)
```Arduino
String text = "अर्दुइनो अच्छा है।";
String searchText = "अच्छा";
String replaceText = "शानदार";

// सर्च और रिप्लेस
text.replace(searchText, replaceText);
Serial.begin(9600);
Serial.println(text); // "अर्दुइनो शानदार है।" प्रिंट होगा
```

## गहराई से जानकारी (Deep Dive)

सर्च और रिप्लेस फंक्शन का इस्तेमाल टेक्स्ट प्रोसेसिंग से लेकर बड़े डाटा रिफैक्टरिंग प्रोजेक्ट्स तक में होता है। शुरुआत में, यह कार्य मैन्युअली होता था। बाद में, रिगुलर एक्सप्रेशंस जैसी टेक्नीक का विकास हुआ जो इसे और आसान बनाती है। अर्दुइनो में, `String` ऑब्जेक्ट का `replace()` मेथड सरल है लेकिन पावरफुल भी है, यह एक ग्लोबल सर्च और रिप्लेस को सपोर्ट करता है। वैकल्पिक तरीके में `char` एरेज़ और `strncpy()` जैसे फंक्शन शामिल हैं जो मेमोरी प्रबंधन पर ज्यादा नियंत्रण देते हैं।

## संबंधित स्रोत (See Also)

- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/
- Arduino और रेगुलर एक्सप्रेशंस: https://www.regular-expressions.info/arduino.html
- C++ `std::string` फंक्शन्स: https://www.cplusplus.com/reference/string/string/
