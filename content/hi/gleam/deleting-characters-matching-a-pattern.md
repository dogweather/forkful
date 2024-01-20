---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
मेलें पैटर्न मिलने वाले वर्णों को हटाना मतलब है कि हम एक निश्चित नमूने के साथ मेल खाने वाले अक्षरों को हटा देते हैं। प्रोग्रामर इसे करते हैं क्योंकि यह उन्हें डेटा को ताजगी से और कुशलता से संभालने देता है।

## कैसे करें:
यहां Gleam का उदाहरण है जिसमें हम एक स्ट्रिंग से विशेष अक्षर हटा रहे हैं:

```Gleam
fn delete_chars(string: String, to_delete: String) -> String {
  string
  |> string.replace(to_delete, "")
}

let result = delete_chars("नमस्ते दुनिया", " ")
assert result == "नमस्तेदुनिया"
```

## गहराई में:
**ऐतिहासिक संदर्भ:** वर्ण हटाने की क्रिया को शुरुआती समय से ही इस्तेमाल किया जा रहा है।

**विकल्प:** ध्यान दें, कि कुछ अवस्थाओं में आप चाहते हैं कि मैच करने के बजाय अनमिलान वर्ण हटाए जाएं, इसके लिए आप replace() функцию 'in' के साथ उपयोग कर सकते हैं।

**उपहार:** जब आप कोड में वर्ण हटाते हैं, तो आपके पास वर्णों केवल वापसी मान होती है, लेकिन मूल स्ट्रिंग प्रभावित नहीं होती है।

## यदि आप और जानना चाहते हैं:
1. ऑफिशियल Gleam डॉक्यूमेंटेशन: [https://gleam.run/docs](https://gleam.run/docs)
2. 'String manipulation in Gleam' ट्यूटोरियल: [https://gleam.run/news/strings-and-their-professors/](https://gleam.run/news/strings-and-their-professors/)