---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट को ढूंढना और  बदलना मतलब किसी खास पाठ स्ट्रिंग को दूसरे पाठ स्ट्रिंग से बदलना। यह एक महत्वपूर्ण कार्य होता है जबकि कंप्यूटर प्रोग्रामिंग में दोहराने या अनुप्रेष्य टेक्स्ट को बदला जाना चाहिए।

## कैसे करें:
Gleam में, आप टेक्स्ट को खोजने और बदलने के लिए `replace` फ़ंक्शन का उपयोग कर सकते हैं। इसका उदाहरण निचे दिया हुआ है:

```Gleam
import gleam/regex

fn replace_text() {
   let text = "यह Gleam ट्यूटोरियल है"
   let updated_text = regex.replace(~"ट्यूटोरियल", text, ~"गाइड")
   assert updated_text == Ok("यह Gleam गाइड है")
}
```

## गहरी जांच:
शुरुआत में, पाठ स्थानांतरण के लिए विभिन्न अनुकूलित कार्यों का निर्माण किया गया था। लेकिन, इसे संगठनात्मक करने और त्रुटियों को कम करने के लिए, `replace` फ़ंक्शन का उपयोग किया जाता है। इसके विकल्प में `split` और `join` आदान-प्रदान का उपयोग करके पाठ को बदलना शामिल है। इसके अंतर्गत यह शोध स्ट्रिंग के आभासी प्रतिष्ठान को प्रवेश करता है और प्रत्येक मिलान को बदलने का कार्य करता है।

## देखें भी:
4. [Replace function in text processing](https://en.wikipedia.org/wiki/Text_processing)