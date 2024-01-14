---
title:                "Rust: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी आधुनिक या समृद्ध भाषा में नए भाषाई कार्यों को समझने और सीखने का सबसे अच्छा तरीका है, उन्हें वास्तविक कोडिंग में लागू करके। तो हम आज जानेंगे कि Rust में temporary file कैसे बनाते हैं। अगर आप Rust प्रोग्रामिंग में नए हैं, तो आपको यह जानना जरूरी है कि temporary file क्या है और आप इसे कैसे इस्तेमाल कर सकते हैं। 

## कैसे करें

Temporary file को बनाने के लिए हम आरंभ में एक फाइल फ़ंक्शन से शुरू करते हैं। इसके बाद हम विभिन्न Rust लाइब्रेरी और कार्य प्रवाहों का उपयोग करते हैं जैसे कि tempfile और std::fs। नीचे दिए गए कोड ब्लॉक में आप इस कोड को देख सकते हैं:

```Rust
use std::fs::File;
use tempfile::tempfile;

fn main() {
   let temp_file = tempfile().unwrap();
   let mut f = File::create(temp_file).unwrap();
   f.write("Hello World!");
}
```
यहां हमने पहले tempfile लाइब्रेरी को इम्पोर्ट किया है और एक temporary file के लिए एक ऑब्जेक्ट लिया है। इसके बाद हमने File लाइब्रेरी से एक नयी फ़ंक्शन create() का उपयोग करके temporary file को बनाया है। अंत में हमने विभिन्न काम को करने के लिए temporary file का उपयोग किया है। 

## डीप डाइव

Temporary file बनाने से पहले हमेशा यह ध्यान रखना चाहिए कि इसमें सुरक्षित तरीके से डाटा को लिखा जाए। आपको अपने कोड में error handling को शामिल करना होगा ताकि आपको किसी भी प्रकार की अनुचित हालत का सामना न करना पड़े। Rust में error handling को बहुत सरल बनाया गया है, आप उसे try/catch के जैसे इस