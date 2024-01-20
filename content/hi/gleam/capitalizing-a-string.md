---
title:                "एक स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Gleam: एक स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "एक स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को कैपिटलाइज करने का अर्थ होता है उसके हर शब्द के पहले अक्षर को बड़ा बनाना। प्रोग्रामर्स इसे डाटा को एकसरह और पठनीय बनाने के लिए करते हैं। 

## कैसे करें:

```Gleam
import gleam/string

pub fn capitalise_string(input: String) -> String {
  string.capitalise(input)
}

assert capitalise_string("नमस्ते दुनिया") == "नमस्ते दुनिया"
```

## गहरी जांच:

- ऐतिहासिक संदर्भ: स्ट्रिंग कैपिटलाइज़ेशन का क्रियान्वयन उच्च-स्तरीय प्रोग्रामिंग भाषाओं से शुरू हुआ था, जो मानव-योग्य योग्यताओं को बढ़ाते थे।
- विकल्प: कैपिटलाइज़ेशन के बिना भी, आप इसी तरह की उम्मीदें निर्धारित कर सकते हैं। आपको स्ट्रिंग के टेक्स्ट केस पर निर्भर नहीं होना चाहिए।
- शासन विवरण: Gleam के ``string.capitalise`` फ़ंक्शन का उपयोग करके, हम हर शब्द के पहले अक्षर को कैपिटलाइज करते हैं।

## और देखिए:

- Gleam Docs: [String](https://gleam.run/documentation/the-gleam-standard-library/std/string/)
- संबंधित स्रोत: [Learning Gleam](https://gleam.run/getting-started/)