---
title:                "स्ट्रिंग इंटरपोलेशन"
date:                  2024-01-20T17:51:14.670848-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग इंटरपोलेशन एक विधि है जहां हम वेरिएबल्स और एक्सप्रेशन्स को सीधे स्ट्रिंग्स के अंदर डालते हैं। इससे कोड पढ़ने में आसान होता है, और स्ट्रिंग में डायनामिक डेटा जोड़ने के लिए यह ज्यादा सहज तरीका है।

## How to: (कैसे करें:)

```gleam
fn main() {
  let name = "विशाल"
  let age = 25
  let greeting = "नमस्ते, मेरा नाम \(name) है और मेरी उम्र \(age) साल है."
  io.print(greeting)
}
```

उपरोक्त कोड स्निपेट की आउटपुट होगी:

```
नमस्ते, मेरा नाम विशाल है और मेरी उम्र 25 साल है.
```

## Deep Dive (गहराई से जानकारी)

स्ट्रिंग इंटरपोलेशन की अवधारणा 1960 के दशक से है, जब प्रोग्रामिंग भाषाएं और टेम्पलेटिंग सिस्टम इसे इम्प्लीमेंट कर रहे थे। इसके विकल्प में फॉर्मैटिंग फंक्शन्स और स्ट्रिंग कॉन्सट्रक्शन शामिल हैं जैसे कि `String.concat` और `printf`. Gleam में स्ट्रिंग इंटरपोलेशन यूजर-फ्रेंडली और त्वरित तरीके से डायनामिक स्ट्रिंग्स बनाता है, जहां एस्केप सीक्वेंस `\()` के अंदर एक्सप्रेशन या वेरिएबल्स को रखकर उन्हे स्ट्रिंग के अंदर इवाल्यूएट किया जाता है।

## See Also (और भी देखें)

- Gleam's official documentation on strings: [Gleam Strings](https://gleam.run/book/tour/strings.html)
- Rust language's take on string interpolation for comparison: [Rust String Formatting](https://doc.rust-lang.org/std/fmt/)
- A comprehensive look at string interpolation in various programming languages on Wikipedia: [String Interpolation on Wikipedia](https://en.wikipedia.org/wiki/String_interpolation)
