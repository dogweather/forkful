---
title:                "Rust: अस्थायी फाइल बनाना"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# क्यों

"अस्थायी फ़ाइल" उत्पन्न करने का कारण क्या है? यह एक उपयोगी तकनीक है जो आपको अपने रस्त प्रोग्राम में अस्थायी डेटा या फाइल को स्थायी रूप से स्थानांतरित करने की अनुमति देता है। अस्थायी फ़ाइल भी मशीन संसाधनों का खर्च कम करता है क्योंकि इसे बनाने के बाद यह स्वचालित रूप से हटा दिया जाता है।

## कैसे करें

यदि आप रस्त में एक अस्थायी फ़ाइल बनाना चाहते हैं, तो आपको सबसे पहले इसके लिए आवश्यक अनुप्रयोगों के साथ अपने कोड में उन्हें शामिल करना होगा। इसके बाद, आपको `std::fs::File::create()` फ़ंक्शन का उपयोग करके अस्थायी फ़ाइल को बनाना होगा। उदाहरण के लिए, नीचे दिए गए कोड ब्लॉक में एक अस्थायी फ़ाइल बनाने का प्रियक्षण दिखाया गया है।

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut temp_file = File::create("temp_file.txt").expect("Unable to create file");
    temp_file.write_all(b"Hello World!").expect("Unable to write to file");
    // temp_file is automatically deleted once it goes out of scope
}
```

इस कोड का 'Outupt' एक फाइल होगी जिसका नाम "temp_file.txt" होगा और जिसमें "Hello World!" लेखा होगा। ज्यादातर अस्थायी फ़ाइलों को बनाने का प्रियक्षण संसाधनों को स्थानांतरित करने के लिए उपयोग किए जाते हैं, लेकिन आप उसे अपने अन्तर्निहित उपयोगकर्ता डेटा को संग्रहीत करने के लिए भी अपने कोड में शामिल कर सकते हैं।

## गहरी तलाश

अस्थायी फ़ाइल बनाने