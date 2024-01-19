---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "C++: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# अस्थायी फ़ाइलें रस्ट में: एक प्रारंभिक गाइड

## क्या और क्यों?

अस्थायी फ़ाइल एक फ़ाइल होती है जिसे एकबार उपयोग करने के बाद हटा दिया जाता है। प्रोग्रामर्स इसे तब बनाते हैं जब उन्हें डेटा को अस्थायी रूप से संग्रहित करने की आवश्यकता होती है।

## कैसे करें:

निम्न उदाहरण में दिखाया गया है कि कैसे रस्ट का उपयोग करके एक अस्थायी फ़ाइल बनाई जा सकती है।

```Rust
use std::fs::File;
use std::io::Write;
use std::env::temp_dir;

fn main() {
    let mut temp_file_path = temp_dir();
    temp_file_path.push("temp_file.txt");

    let mut file = File::create(&temp_file_path).expect("could not create file");

    file.write_all(b"Hello, world!").expect("unable to write");

    println!("Created a temp file: {:?}", &temp_file_path);
}
```

यहां `temp_dir` और `File::create` फ़ंक्शन्स का उपयोग होता है जो एक अस्थायी फ़ाइल बनाते हैं। `write_all` इस फ़ाइल में लिखने के लिए होता है। 

## गहराई से समझना

रस्ट में अस्थायी फ़ाइलें बनाना एक आम और आसान तरीका है, लेकिन इसका उपयोग दुर्लभ परिस्थितियों में होता है, जैसे कि गेटबिट्स प्रोग्रामिंग या उन प्रक्रियाओं में जहां डेटा को एक बार का उपयोग करके बाहर फेंकना है। हालांकि, इसे बहुत सूक्ष्मता से उपयोग करना आवश्यक है, क्योंकि यदि इसका उपयोग सही ढंग से नहीं किया जाता है, तो यह सिस्टम की फ़ाइल स्थाना क्षमता खत्म कर सकती है। इसके अलावा, डिस्क स्पेस का उपयोग बचत करने का एज़ीर तरीका `Vec<u8>` या बाइनरी बफ़र का उपयोग करना है, जिससे अस्थायी स्टोरेज डिस्क पर नहीं होती है।

## अधिक जानने के लिए

रस्ट के बारे में और अधिक जानने के लिए, आप निम्न लिंक पर जा सकते हैं:

1. [Rust डॉक्यूमेंटेशन](https://doc.rust-lang.org/book/)
2. [Tempfile crate डॉक्यूमेंटेशन](https://docs.rs/tempfile/3.2.0/tempfile/)
3. [Rust File IO - फ़ाइलों को लिखने और पढ़ने](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-io-idioms.html)