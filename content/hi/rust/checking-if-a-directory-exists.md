---
title:                "एक निर्देशिका मौजूद है या नहीं जांचना"
html_title:           "Lua: एक निर्देशिका मौजूद है या नहीं जांचना"
simple_title:         "एक निर्देशिका मौजूद है या नहीं जांचना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी मौजूद है या नहीं, इसकी जांच करना इसका तय करने का तरीका होता है कि क्या आपका प्रोग्राम किसी विशेष डायरेक्टरी का उपयोग कर सकता है या नहीं। किसी संगठन के लिए महत्वपूर्ण डाटा गुम न करने या ओवरराइट करने के लिए, यह जांच सुनिश्चित करना महत्वपूर्ण हो सकता है। 

## कैसे करें: 

Rust प्रोग्रामिंग में, आप `std::fs` के `metadata` फंक्शन का उपयोग करके इसे कर सकते हैं। 

```Rust
use std::fs;

fn main() {
    let path = "/some/path/to/directory";
    let metadata = fs::metadata(path);
    match metadata {
        Ok(data) => {
            if data.is_dir() {
                println!("{} is a directory", path);
            } else {
                println!("{} is not a directory", path);
            }
        },
        Err(_error) => println!("{} does not exist", path),
    }
}
```

यह कोड `"/some/path/to/directory"` पर डायरेक्टरी होने की जांच करता है, और यदि वह मौजूद है, तो यह प्रिंट करता है कि वह डायरेक्टरी है, नहीं तो यह प्रिंट करता है कि वह मौजूद नहीं है। 

## गहराई में: 

Rust भाषा ने C++ और अन्य प्राचीन भाषाओं से प्रेरणा लेने के बाद ही `std::fs::metadata` जैसे विचार विकसित किए। इसे कोर और सुरक्षित भाषा की संरचना को बनाये रखने के लिए डिज़ाइन किया गया है। 

`std::fs::metadata` के विकल्प में `std::path::Path` का `exists` फंक्शन शामिल है, जो सीधे पथ की अस्तित्व की जाँच करता है, लेकिन यह डायरेक्टरी है या नहीं, इसका निर्धारण नहीं करता है। 

डायरेक्टरी की मौजूदगी की जांच करने का तरीका ऑपरेटिंग सिस्टम और फ़ाइल सिस्टम पर आधारित होता है, जिसमें सिस्टम कॉल का उपयोग होता है। 

## देखें भी: 

- Rust की आधिकारिक दस्तावेज़ीकरण: https://doc.rust-lang.org/std/fs/fn.metadata.html 
- डायरेक्टरी की अस्तित्व की जाँच का विकल्प: https://doc.rust-lang.org/std/path/struct.Path.html#method.exists