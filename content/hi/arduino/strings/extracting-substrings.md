---
date: 2024-01-20 17:45:21.176227-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092F\u0939\
  \u093E\u0902, `\"Arduino\"` \u0935\u0939 substring \u0939\u0948 \u091C\u094B \u0939\
  \u092E\u0928\u0947 `sentence` \u0938\u0947 index 7 \u0938\u0947 14 \u0924\u0915\
  \ \u0928\u093F\u0915\u093E\u0932\u0940 \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:54.717850-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092F\u0939\u093E\u0902\
  , `\"Arduino\"` \u0935\u0939 substring \u0939\u0948 \u091C\u094B \u0939\u092E\u0928\
  \u0947 `sentence` \u0938\u0947 index 7 \u0938\u0947 14 \u0924\u0915 \u0928\u093F\
  \u0915\u093E\u0932\u0940 \u0939\u0948\u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## How to: (कैसे करें:)
```Arduino
String sentence = "Hello, Arduino World!";
String sub = sentence.substring(7, 14);

Serial.begin(9600);
Serial.println(sub);  // Output: "Arduino"
```
यहां, `"Arduino"` वह substring है जो हमने `sentence` से index 7 से 14 तक निकाली है।

```Arduino
String anotherExample = "Extract this part!";
String extracted = anotherExample.substring(8);

Serial.begin(9600);
Serial.println(extracted);  // Output: "this part!"
```
इसमें, केवल starting index देने से, end तक का पूरा text extract हो जाता है।

## Deep Dive (गहराई में जानकारी)
Substring निकालने की ability बहुत early programming languages जैसे की C में भी थी। Arduino अपने `String` class में `substring()` function प्रदान करता है।

Alternatives में `strtok()`, `strstr()`, or custom functions जैसे की `char arrays` पर loops चलाना शामिल हैं। पर, `String` class का `substring()` function easy और error-free होता है।

Implementation के लिए, `substring()` function start index और end index (optional) लेता है। अगर end index provide नहीं किया जाये तो यह string के end तक extract कर लेता है। Memory management और large strings के efficient handling को ध्यान में रखना ज़रूरी है।

## See Also (और देखें)
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino String Manipulation Tips: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
- The Evolution of String functions in C: https://en.wikipedia.org/wiki/C_string_handling
