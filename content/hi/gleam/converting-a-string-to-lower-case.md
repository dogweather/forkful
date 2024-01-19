---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों

स्ट्रिंग को लोअरकेस में कन्वर्ट करना मतलब होता है की स्ट्रिंग के सभी अक्षरों को छोटे अक्षर में बदलना। प्रोग्रामर्स इसे तब करते हैं जब वे डाटा की संवेदनशीलता को कम करना चाहते हैं या तब जब उनको यकीन दिलाना होता है कि वो सही तरीके से डाटा का मिलान कर रहे हैं।

## कैसे करें

Gleam में, हम `string.to_lower` फ़ंक्शन का इस्तेमाल करके अपने स्ट्रिंग को लोअरकेस में बदल सकते हैं। 

जैसे कि:

```gleam
import gleam/string

fn main() {
  let message = "Hello WORLD!"
  let lower_case_message = string.to_lower(message)
  println(lower_case_message) // "hello world!"
}
```

इस मामले में, हमारी "Hello WORLD!" स्ट्रिंग "hello world!" में बदल जाएगी।

## गहराई में

1. **ऐतिहासिक प्रसंग:** जब ये विचार किया जाता है कि क्या सभी कैपीटल मात्राएं लोअरकेस में हैं या नहीं, यह काफी जटिल हो सकता है। यह उन भाषाओं पर निर्भर करता है जो प्रोग्रामर का उपयोग कर रहा है। Gleam में, यह Unicode स्ट्रिंगों के साथ काम करने का क्षमता देता है।

2. **विकल्प:** जिसे कुछ अन्य भाषाओं में `tolower()`, `LowerCase()`, आदि के रूप में जाना जाता है। यह वास्तव में किसी भी भाषा के भितर विभाज्यताओं को मान्यता देता है।

3. **विन्यास विवरण:** `string.to_lower` फ़ंक्शन को एक Unicode स्ट्रिंग प्रदान करने की अनुमति होती है और वह सभी बड़े अक्षरों को उनके संबंधित छोटे अक्षरों में परिवर्तित करता है हालांकि, विशेष मामलों में जहां बड़ा और छोटा अक्षर नहीं मिलता, अक्षरों को अपरिवर्तित छोड़ दिया जाता है। 

## और भी देखें

- [Gleam String Docs](https://gleam.run/book/tour/strings.html)
- [Unicode Lowercase Conversion Issues](https://unicode.org/faq/casemap_charprop.html#11)