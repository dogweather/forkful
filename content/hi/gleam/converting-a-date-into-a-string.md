---
title:                "Gleam: तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

एक तारीख को स्ट्रिंग में रूपांतरित करने का उपयोग क्यों किया जाता है, इसके पीछे का कारण क्या है।

## कैसे करें

यहां आप ग्लिम कोड के माध्यम से तारीख को स्ट्रिंग में रूपांतरित करने के लिए कुछ उदाहरण अन्य समाप्तियां देख सकते हैं।

```Gleam
let date = Date.from_parts(2021, 9, 26)

// तारीख को स्ट्रिंग में रूपांतरित करने के लिए विभिन्न मूल्यों का प्रयोग किया जा सकता है।
let string_date = date |> Date.format("{day}-{month}-{year}")
let string_month = date |> Date.format("{month}")
let string_full = date |> Date.format("{day} {month}, {year}")

// उत्पाद आउटपुट:
string_date = "26-9-2021"
string_month = "September"
string_full = "26 September, 2021"
```

## गहराई में जाएं

तारीख को स्ट्रिंग में रूपांतरित करने के लिए ग्लीम में डेटा और स्ट्रिंग प्रकार का प्रयोग कैसे किया जाता है, इससे आपको कोड को और बेहतर समझने में मदद मिलेगी। भागीदारिता भागीदारिता कैसे कार्य करती है और टेम्पलेट वार्ग / स्क्रिप्ट कैसे काम करते हैं, इससे आपको बेहतर समझने में मदद मिलेगी।

## देखें भी

[ग्लीम डेटा टाइप्स](https://gleam.run/book/types.html#date)
[ग्लीम स्ट्रिंग फंक्शंस](https://gleam.run/book/stdlib.html#string-function)
[ग्लीम फार्मेटिंग](https://gleam.run/book/stdlib.html#format-functions)