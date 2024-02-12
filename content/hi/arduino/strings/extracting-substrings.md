---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:21.176227-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Substring का मतलब है किसी string का एक छोटा हिस्सा निकालना। Programmers इसे use करते हैं डेटा को analyze करने, manipulate करने या किसी specific text को extract करने के लिए।

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
