---
title:                "Rust: टेक्स्ट फाइल पढ़ना"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
क्या आपने कभी किसी टेक्स्ट फ़ाइल को पढ़ने का प्रयास किया है? यदि हाँ, तो आप जानते होंगे कि प्रोग्रामिंग वर्ल्‍ड में टेक्स्ट फ़ाइलों को पढ़ने का काम बहुत ही अहम हो सकता है। इस ब्लॉग पोस्ट में हम रास्ट प्रोग्रामिंग भाषा के माध्यम से अपनी टेक्स्ट फाइलों को पढ़ने के बारे में जानेंगे।

## कैसे करें
टेक्स्ट फ़ाइलों को पढ़ने के लिए पहला कदम है, उन्हें खोलना। हमने एक नमूना टेक्स्ट फ़ाइल बनाई है और उसमें कुछ लेखन शामिल किए हैं। नीचे दिए गए रास्ट कोड ब्लॉक में हमने दिखाया है कि कैसे हम टेक्स्ट फ़ाइल को खोल सकते हैं और उसकी सामग्री को पढ़ सकते हैं।

```Rust
use std::fs::File;

fn main() {
    // Open the file
    let file = File::open("example.txt");
    
    // Read the contents and print
    if let Ok(file) = file {
        let contents = std::fs::read_to_string(file).expect("Unable to read file");
        println!("File contents:\n{}", contents);
    } else {
        println!("Unable to open file");
    }
}
```

इस कोड के प्रयोग से हमारे पास निम्नलिखित आउटपुट होगा:

```
File contents:
This is a sample text file.
It contains some writing for demonstration purposes.
```

समझने के लिए, हमने ```expect()``` फ़ंक्शन का उपयोग किया है ताकि यदि फ़ाइल नहीं खुल पाई जाती है, तो हमें एक संदेश मिल सके। आप अपने स्वयं के विकल्पों को उपयोग कर सकते हैं और समझ सकते हैं कि आपको कोड को कैसे तख्ती के अंदर लिखना है।

## गहराई में जाएं
अब हमें यह बताने की जरूरत है कि टेक्स्ट फ़ाइल वास्तव म