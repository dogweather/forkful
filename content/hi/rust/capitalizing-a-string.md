---
title:                "Rust: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

वस्तुतः, रस्ट प्रोग्रामिंग भाषा में एक स्ट्रिंग को कैपिटलाइज करने की आवश्यकता उस समय होती है जब हमें उस स्ट्रिंग को बड़े हरूफों में देखना होता है। उदाहरण के लिए, किसी टेक्स्ट संशोधन प्रोग्राम में उपयोगकर्ता द्वारा दिया गया पासवर्ड स्ट्रिंग को कैपिटलाइज किया जाता है ताकि उसे सुलभता से पढ़ा जा सके।

## कैसे करें

```Rust
// एक स्ट्रिंग को कैपिटलाइज करने का सरल उपाय
let name = String::from("rust");
let capitalized_name = string::to_uppercase(name);
println!("{}", capitalized_name);

// अंग्रेजी का अक्षर ऊपरी किस्म को छन्दस्वर रूप में बदलता है
let letter = 'a';
let capital_letter = char::to_uppercase(letter);
println!("{}", capital_letter);
```

उपरोक्त कोड ब्लॉक एक स्ट्रिंग वर्णन को कैपिटलाइज करने और एक अक्षर को उसके ऊपरी किस्म में बदलने के उपाय दर्शाता है। परिणाम निम्नलिखित है:

```bash
RUST
A
```

## गहराई तक

स्ट्रिंग को कैपिटलाइज करने के पीछे कारण, इसे पढ़ने और बनाने के लिए प्रोग्रामरों को सुलभ बनाना है। यह एक स्ट्रिंग तकनीक है जो अक्षरों को उनकी ऊपरी या निचली किस्म में बदलने के कार्य करती है। इसके अलावा, यह जानकार और अनुभवी प्रोग्रामरों के लिए बहुत उपयोगी है जो शामिल स्ट्रिंग तकनीकों को समझने के लिए संतुआप्त हैं।

## देखें भी

- [Rust स्ट्रिंग वर्णन](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust अक्षर तकनीक](https://doc.rust-lang.org/std/primitive.char.html)
- [कैपिटलाइ