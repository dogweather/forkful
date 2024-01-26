---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:08.392334-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या & क्यों?
String की लंबाई पता करना यानी string में मौजूद characters की संख्या जानना। Programmers इसे इसलिए करते हैं क्योंकि data validation, text processing, या UI elements को properly display करने के लिए यह जानकारी जरूरी होती है।

## कैसे करें:
यहाँ Gleam कोड दिया गया है जिससे आप string की लंबाई पा सकते हैं:

```gleam
fn main() {
  let greeting = "नमस्ते!"
  let length = string.len(greeting)
  io.println("String length is: " ++ int.to_string(length))
}
```

जब आप इस कोड को run करेंगे, आपको निम्न output मिलेगा:

```
String length is: 7
```

## गहराई से जानकारी:
प्राचीन समय से ही, जब भी किसी भाषा में text processing की गई है, string length की जानकारी महत्वपूर्ण रही है। Gleam में, `string.len` function का उपयोग करके आसानी से इसे प्राप्त किया जा सकता है। UTF-8 encoding के कारण, विभिन्न characters का size अलग हो सकता है, लेकिन `string.len` हमेशा सही character count देता है। अन्य भाषाओं में, इस प्रक्रिया को कभी-कभी अधिक complex algorithms की जरूरत हो सकती है, खासकर जब यह special characters या emojis की बात आती है। Gleam में, यह सब बैकएंड में होता है, इसलिए developers के लिए यह बहुत सीधा है।

## संबंधित सूत्र:
- Gleam language official documentation: [https://gleam.run/](https://gleam.run/)
- Unicode string processing information: [https://unicode.org](https://unicode.org)
