---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
मानक त्रुटि (standard error) में लिखना प्रोग्राम की गलतियाँ संदेशों को दिखाने का तरीका है. प्रोग्रामर इसका इस्तेमाल एरर लॉग को मानक आउटपुट से अलग रखने के लिए करते हैं ताकि डीबगिंग आसान हो सके.

## कैसे करें? (How to:)
```gleam
import gleam/io

fn main() {
  io.print("यह मानक आउटपुट पर दिखेगा")
  io.eprint("यह मानक त्रुटि पर दिखेगा")
}
```
सैंपल आउटपुट:
```
यह मानक आउटपुट पर दिखेगा
यह मानक त्रुटि पर दिखेगा
```

## गहराई से जानकारी (Deep Dive)
मानक त्रुटि स्ट्रीम (stderr) को पहली बार UNIX ऑपरेटिंग सिस्टम में पेश किया गया था. इसका मकसद था एरर मैसेजेस को मानक आउटपुट (stdout) से अलग करना. विकल्पों में लॉग फाइलों में लिखना या एक्सटर्नल लॉगिंग सिस्टम्स का इस्तेमाल करना शामिल है. ग्लीम में, stderr को `io.eprint` या `io.eprint_line` फंक्शन के जरिए इम्प्लीमेंट किया जाता है.

## और भी (See Also)
- [Unix standard streams explanation](https://en.wikipedia.org/wiki/Standard_streams)
