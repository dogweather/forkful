---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल लिखना का मतलब है डाटा को टेक्स्ट फॉर्म में सेव करना। प्रोग्रामर्स इसे डाटा स्टोरेज, लॉगिंग, कॉन्फिगरेशन्स और यूज़र जनरेटेड कंटेंट के लिए इस्तेमाल करते हैं।

## कैसे करें:

Gleam में यूँ करें:

```gleam
import gleam/io
import gleam/result.{Result}

pub fn write_to_file(contents: String) -> Result(Nil, String) {
  try file from io.open("greeting.txt", [io.Write])
  io.write(file, contents)
  io.close(file)
}

fn main() -> Nil {
  case write_to_file("नमस्ते दुनिया!") {
    Ok(_) -> io.println("फाइल सेव हो गई है।")
    Error(err) -> io.println("एरर: " ++ err)
  }
}
```

जब आप उपरोक्त ग्लीम कोड रन करेंगे, तब एक 'greeting.txt' नाम की टेक्स्ट फाइल बनाई जाएगी जिसमें "नमस्ते दुनिया!" लिखा होता है।

## गहराई में:

टेक्स्ट फाइल लिखने का कांसेप्ट काफी पुराना है और कंप्यूटर के शुरुआती दिनों से ही है। इसके अल्टरनेटिव्स में डाटाबेस, बाइनरी फाइल्स और क्लाउड स्टोरेज शामिल हैं। Gleam में `io` मॉड्यूल का इस्तेमाल करके आसानी से फाइल सिस्टम के साथ इंटरेक्ट किया जा सकता है, जिसमें टाइप सेफ्टी और एरर हैंडलिंग भी शामिल है।

## और भी जानें:

- Gleam ऑफिशियल डॉक्यूमेंटेशन: [Gleam Lang](https://gleam.run)

यह लेख Gleam के वर्तमान वर्शन (ज्ञान कटौती के समय) पर आधारित है। Gleam के अपडेट्स और बदलावों के लिए, प्लीज उसकी ऑफिशियल साइट पर जाएँ।
