---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

'मिलान का पैटर्न' के अनुसार अक्षरों को हटाना, इसका मतलब होता है की किसी विशिष्ट पैटर्न के अनुसार स्ट्रिंग से क्यरेक्टर्स हटाना। प्रोग्रामर्स इसे करते हैं क्योंकि इससे डाटा संशोधन और मैनिपुलेशन में सहायता मिलती है।

## कैसे करें:

निम्नलिखित रस्ट कोड स्ट्रिंग "Hello, World!" से सब ही कोमाओं को हटा देगा:

```Rust
fn main() {
    let hello_world = "Hello, World!";
    let result = hello_world.replace(",", "");
    println!("{}", result);
}
```

आउटपुट:

```Rust
Hello World!
```

## गहराई में:

यह कार्य (किरदारों को हटाने का) अक्सर डाटा क्लीनिंग और मैनिपुलेशन में उपयोग होता है, जिसके इतिहास में यह एक अविश्वसनीय साधन रहा है। Rust के अलावा, अन्य भाषाओं जैसे की Python, Java आदि में भी इसके तत्ववत वास्तु होते हैं। इसे "String.replace()" method का उपयोग करके संवारा जा सकता है, जो कि रस्ट की बुनियादी भाषा लाइब्रेरी में रहता है।

## देखने के लिए:

1. Rust की आधिकारिक डॉक्यूमेंटेशन: https://doc.rust-lang.org/std/
2. Rust का बेसिक स्ट्रिंग मनिपुलेशन: https://www.tutorialspoint.com/rust/rust_strings.htm
3. Rust की पुरी जानकारी: https://learning-rust.github.io/docs/a3.handling_strings.html