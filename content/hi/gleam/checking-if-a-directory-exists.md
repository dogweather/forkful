---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:56:41.394121-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी मौजूद है या नहीं यह जांचना क्या है? यह एक चेक है जो बताता है कि फाइल सिस्टम में एक डायरेक्टरी है भी या नहीं। प्रोग्रामर्स इसे क्यों करते हैं? एरर्स को रोकने और डाटा गुमने से बचाने के लिए।

## How to (कैसे करें)
```gleam
// मान लो हमारे पास stdlib में एक fs मॉड्यूल है
import gleam/stdlib/fs

pub fn check_directory_exists(path: String) -> Bool {
  fs.dir_exists(path)
}

// इस्तेमाल करने का उदाहरण:
let directory_exists = check_directory_exists("my_folder")
assert directory_exists == True
```
अगर "my_folder" मौजूद है, तो `directory_exists` में `True` होगा।

## Deep Dive (गहराई से समझें)
डायरेक्टरी जांचने की विधियां बहुत सालों से हैं। पहाड़े, `exists` सिस्टम कॉल्स इसके लिए इस्तेमाल होते थे। ग्लीम में, हम stdlib के fs मॉड्यूल का इस्तेमाल करते हैं, जिसे आसानी से समझा और इस्तेमाल किया जा सकता है।
विकल्पों के तौर पर, कुछ सिस्टम्स में शायद अन्य तरीके होंगे, जैसे कि सिस्टम कमांड्स या थर्ड-पार्टी पैकेजेस जो विभिन्न ऑपरेटिंग सिस्टम्स के लिए हों।
अगर हम गहराई से बात करें तो, `fs.dir_exists` आमतौर पर निचले स्तर के सिस्टम एपीआई को कॉल करता है और सुनिश्चित करता है कि डायरेक्टरी एक्सेसिबल हो।

## See Also (और जानें)
- [Gleam stdlib documentation](https://hexdocs.pm/gleam_stdlib/)