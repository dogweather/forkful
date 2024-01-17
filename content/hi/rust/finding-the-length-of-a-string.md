---
title:                "शब्द की लंबाई ढूंढना"
html_title:           "Rust: शब्द की लंबाई ढूंढना"
simple_title:         "शब्द की लंबाई ढूंढना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

आमतौर पर प्रोग्रामर्स को स्ट्रिंग की लंबाई को पाने की जरूरत होती है, जो हमारे लेख द्वारा कोडिंग को पढ़ने से प्राप्त की जाती है। स्ट्रिंग की लंबाई का मतलब होता है कि उसमें कितने अक्षर हैं या कितने ध्वनि ब्लॉक हैं।

## क्या और क्यों?

किसी भी प्रोग्रामिंग भाषा में, हम डेटा को सहेजने के लिए स्ट्रिंग का उपयोग करते हैं। जब हमें स्ट्रिंग की लंबाई की जानकारी चाहिए होती है, तो हम कोड कोडिंग में उसका उपयोग करते हैं। यह हमें इसकी पहचान करने और आवश्यकताओं को पूरा करने में मदद करता है।

## कैसे करें:

```Rust
let string_example = "Hello World!";
let string_length = string_example.len();
println!("The length of the string is {}", string_length);
```

इस उदाहरण में, हमने विभिन्न स्ट्रिंग के वापसी योग्यताओं का उपयोग करके एक स्ट्रिंग के लंबाई को प्रिंट करने के लिए एक मैथड का इस्तेमाल किया है। इसमें "Hello World!" रूपांतरित किया जा सकता है और प्रिंट किया जा सकता है।

```Rust
let mut string = String::new();
io::stdin().read_line(&mut string).expect("Failed to read line");
println!("The length of the string is {}", string.len());
```

इस उदाहरण में, प्रोग्राम कुंजीबोर्ड से इनपुट छांटने और स्ट्रिंग के लंबाई को प्रिंट करने के लिए एक मैथड का उपयोग किया गया है। इसमें आप अपने खुद के स्ट्रिंग को स्कैन कर सकते हैं और उसकी लंबाई का साइज जान सकते हैं।

## गहराई में बढ़ें:

स्ट्रिंग की लंबाई को पाने के लिए, प्रोग्रामर्स उपयोग करते हैं std::string::String या std::str मैथोड। लेकिन, कुछ भाषा जैसे जावा में स्ट्रिंग क्लास के साथ सुगमता को उधृत करने की जरूरत होती है।

## और देखें:

स्ट्रिंग की लंबाई पाने के बारे में अधिक जानकारी के लिए नीचे दिए गए स्रोतों को देखें:

https://doc.rust-lang.org/stable/std/primitive.str.html#method.len 

https://www.tutorialspoint.com/rust/rust_strings.htm 

https://www.codingame.com/playgrounds/1654/welcome-to-rust/strings