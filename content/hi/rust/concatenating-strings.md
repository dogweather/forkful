---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# रस्ट प्रोग्रामिंग: स्ट्रिंग्स को जोड़ना

## क्या और क्यों?

स्ट्रिंग्स को जोड़ना (concatenating strings) दो या दो से अधिक स्ट्रिंग्स को एक साथ लगाने का कार्य होता है। प्रोग्रामर्स इसे लगभग हर बार तब करते हैं जब उन्हें निर्धारित पाठ तैयार करना होता है, जैसे उपयोगकर्ता के लिए संदेश। 

## कैसे करें:

रस्ट में स्ट्रिंग्स को जोड़ने के लिए `+` और `format!` ऑपरेटर का उपयोग किया जा सकता है। आइए दोनों के उदाहरण देखें:

```Rust 
    let str1 = "नमस्ते";
    let str2 = " दुनिया";
    let hello_world = str1.to_owned() + str2;
    println!("{}", hello_world);
```
आउटपुट:
```
नमस्ते दुनिया
```
`format!` मैक्रो के साथ:
```Rust 
    let str1 = "नमस्ते";
    let str2 = " दुनिया";
    let hello_world = format!("{}{}", str1, str2);
    println!("{}", hello_world);
```
आउटपुट:
```
नमस्ते दुनिया
```

## गहराई से जानकारी:

रस्ट में स्ट्रिंग्स को जोड़ने के लिए `+` ऑपरेटर और `format!` मैक्रो उपयोगी हैं, लेकिन उनमें अपनी खुद की चुनौतियां हैं। `+` ऑपरेटर उपयोग करने से, बाएं तरफ की स्ट्रिंग को `String` होना होगा, जो कि सभी केसेस में संभव नहीं है। `format!` मैक्रो अधिक वर्सेटाइल है, लेकिन यदि आप केवल स्ट्रिंग्स को जोड़ना चाहते हैं तो यह थोड़ा अधिक हो सकता है।

## अधिक जानकारी के लिए:

1. [रस्ट अधिकारिक दस्तावेज़ीकरण](https://doc.rust-lang.org/std/string/struct.String.html) स्ट्रिंग्स के बारे में और सीखने के लिए  
2. [The Rust Programming Language Book](https://doc.rust-lang.org/book/ch08-02-strings.html#operations-on-strings) स्ट्रिंग्स के बारे में और गहराई से जानने के लिए
3. [रस्ट ब्लॉग](https://blog.rust-lang.org/inside-rust/2020/10/23/What-the-error-handling-project-group-is-working-on.html) स्ट्रिंग्स की लेटेस्ट उपयोगिताओं के बारे में जानने के लिए