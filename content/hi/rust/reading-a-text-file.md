---
title:                "Rust: टेक्स्ट फाइल पढ़ना"
simple_title:         "टेक्स्ट फाइल पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों?

प्रोग्रामिंग में अपनी निशानदही करने के लिए, हमें कभी-कभी सुलभ दस्तावेज़ों से भी काम करना पड़ता है। एक ऐसे समग्रण्ध को समझने से आपका कोड और आपके पास उससे अधिक मिल सकता है।

# कैसे करें

"```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("file.txt").expect("Unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Unable to read file");

    println!("File contents: {}", contents);
}
```

यह उदाहरण में, हमने `File` और `io` मॉड्यूल का उपयोग किया है जो आपको एक फ़ाइल खोलने और उसकी सामग्री को पढ़ने की सुविधा देता है। इसमें हमें `file.txt` नाम की फाइल को खोलने का प्रयास किया जाता है, जो हमारे कोड के संगत फ़ॉर्मेट में उपलब्ध है। आप जिस फ़ाइल को पढ़ना चाहते हैं, उसका नाम इस विधि में उपयोग किया जाता है। इसमें हमने `read_to_string` विधि का प्रयोग किया है जो संग्रहक (buffer) में फाइल की सामग्री को संरचित करता है। अंत में, हम निश्चित रूप से सिक्योरिटी के कारण सभी श्रेणी का उपयोग करते हैं।

# गहराई में

समग्रण्ध पढ़ने के लिए, हमने Rust अपने समग्रण्धक0 का उपयोग किया है। इसमें आप पाठ, संग्रहक आदि देख सकते हैं, जो कैसी प्रकार के लेवल पर स्ट्रीम की शुरुआत की जाती है। इसमें आयतन-पाठ को खोलने के बाहर एक सामने को रखने के आवश्यकताएं भी हैं।

# देखें भी

- [Rust डॉक्यूमेंटेशन](https://doc.rust-lang.org/stable/book