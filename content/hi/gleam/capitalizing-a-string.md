---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग कैपिटलाइज़ेशन का मतलब है हर शब्द के पहले अक्षर को बड़ा (कैपिटल) करना। प्रोग्रामर्स ऐसा यूज़र इंटरफेस, दस्तावेज़ीकरण या डेटा फॉर्मेटिंग के मामले में यूनिफॉर्मिटी और पठनीयता बढ़ाने के लिए करते हैं।

## How to: (कैसे करें:)
Gleam में फिलहाल कोई बिल्ट-इन फ़ंक्शन स्ट्रिंग कैपिटलाइज़ करने के लिए नहीं है। पर आइए हम एक साधारण फ़ंक्शन लिखें जो यह काम कर सकता है:

```Gleam
import gleam/string

fn capitalize(str: String) -> String {
  string.first(str)
  |> result.map(string.to_uppercase)
  |> result.and_then(fn(first) {
    string.drop_left(str, 1)
    |> result.map(fn(rest) { first ++ rest })
  })
  |> result.unwrap_or(str)
}

pub fn main() {
  let example = "hello, ग्लीम दुनिया!"
  capitalize(example)
}
```

सैंपल आउटपुट:

```
"Hello, ग्लीम दुनिया!"
```

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग कैपिटलाइज़ेशन साधारण फ़ंक्शन है, पर इसके पीछे बहुत सोच होती है। पिछले प्रोग्रामिंग भाषाओं में कभी-कभी इसे करना ज़्यादा सीधा होता था, पर यूनिकोड और विभिन्न भाषाओं के अक्षरों के समर्थन के कारण अब जटिलताएँ बढ़ गई हैं। `capitalize` फंक्शन में चरणों का अनुसरण किया जाता है ताकि हर स्टेप स्पष्ट रहे और देखभाल के साथ यूनिकोड हैंडल किया जा सके। ग्लीम में, यह फ़ंक्शनलिटी `gleam/string` मॉड्यूल में `first`, `to_uppercase`, और `drop_left` फ़ंक्शन्स के साथ की जाती है। विकल्पी दृष्टिकोण में रेगुलर एक्सप्रेशन्स या पाइपलाइनिंग में अधिक कॉम्प्लेक्स ट्रांसफॉर्मेशन्स शामिल हो सकते हैं।

## See Also (और जानकारी के लिए):
- Gleam भाषा के डॉक्यूमेंटेशन: [Gleam Language Docs](https://gleam.run/book/)
- यूनिकोड स्ट्रिंग हैंडलिंग: [Unicode String Handling](http://unicode.org/reports/tr29/)