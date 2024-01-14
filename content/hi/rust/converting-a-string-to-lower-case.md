---
title:                "Rust: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी अपने बाहरी मेमोरी में टेक्स्ट को साफ और सरल तरीके से प्रस्तुत करने के लिए कोडिंग कर रहे हो? अगर हाँ, तो आपको अपने वेरिएबल को निर्दिष्ट स्थिति में रखने के लिए lower case में convert करने की जरूरत हो सकती है। यह एक आसान और महत्वपूर्ण काम है जो आपको लंबे टेक्स्ट में भी सुधार प्रदान कर सकता है। 

## कैसे करें

``` Rust
let name = "Rust Programming";
println!("Before converting to lower case: {}", name);
let name_lower = name.to_lowercase();
println!("After conversion: {}", name_lower);
```

**आउटपुट:**

Before converting to lower case: Rust Programming

After conversion: rust programming

ऊपर दिया गया कोड आपको कोई भी टेक्स्ट (उदाहरण के लिए name वेरिएबल को) lower case में convert करने में मदद करेगा। साथ ही साथ, इसमें आप दो अलग-अलग स्थितियों में भी टेक्स्ट को convert कर सकते हैं। आपको बस सही मीथोड को निर्दिष्ट करने की जरूरत होगी।

## गहराई में जाएं

जब आप कोड करने लगते हैं, तो आपको अपनी प्रोग्रामिंग भाषा के साथ गहराई में समझने की जरूरत होती है। इसलिए, उसी तरह से lower case में convert करने के लिए आपको कुछ तकनीकों को भी समझने की जरूरत हो सकती है। यदि आप दो अलग-अलग स्थितियों में एक समान वेरिएबल को lower case में convert करते हैं, तो आपको प्रोग्राम्स को लूप द्वारा लिखने की आवश्यकता नहीं होती है। इसके स्थान पर, आपको फ़ंक्शंस की मदद से भी इस काम को कर सकते हैं। 

## देखें भी

[Official Rust Documentation on String Methods](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase) 

[Rust By Example on Strings](https