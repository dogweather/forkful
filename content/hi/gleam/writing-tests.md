---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेस्ट लिखना यानी अपने कोड की जाँच करना कि वह उम्मीद के मुताबिक काम कर रहा है या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे बग्स का पता चलता है, कोड की क्वालिटी सुधरती है और सॉफ्टवेयर को मेनटेन करना आसान हो जाता है।

## How to: (कैसे करें)
ग्लीम (Gleam) में टेस्ट केस लिखने के लिए आपको यह बेसिक स्टेप्स फॉलो करने होंगे। सबसे पहले आपको एक टेस्ट मॉड्यूल बनानी होगी:

```gleam
// test/my_module_test.gleam
import gleam/should
import my_module

pub fn addition_test() {
  my_module.add(1, 2)
  |> should.equal(3)
}
```

सैंपल आउटपुट:

```
test my_module_test ... ok
All tests passed.
```

इस उदाहरण में `my_module.add` फंक्शन की जाँच `should.equal` का इस्तेमाल करके की जा रही है।

## Deep Dive (गहराई से जानकारी)
पहले के समय में, टेस्ट लिखने के लिए बहुत सिम्पल टूल्स इस्तेमाल होते थे या कई बार प्रोग्रामर्स मैन्युअली टेस्ट करते थे। फिर, TDD (Test-driven Development) जैसे कॉन्सेप्ट्स ने जगह बनाई, जिसमें पहले टेस्ट लिखा जाता है फिर कोड। 

ग्लीम में टेस्ट लिखने के विकल्पों में शामिल हैं `gleam/expects` लाइब्रेरी जिससे आप अधिक सशक्त टेस्ट लिख सकते हैं। ये डिटेल्स लिखने में सहायक होते हैं और टेस्टिंग प्रक्रिया को अधिक रोबस्ट बनाते हैं।
