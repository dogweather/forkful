---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/rust/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:40:16.008692-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String को lower case में बदलना यानी पूरे टेक्स्ट को छोटे अक्षरों में लिखना। Programmers तब ऐसा करते हैं जब case sensitivity महत्वपूर्ण न हो और डेटा को सामान्य रूप से संभालने की जरूरत हो।

## How to: (कैसे करें:)
```Rust
fn main() {
    let greeting = "नमस्ते, दुनिया!";
    let lower_greeting = greeting.to_lowercase();
    println!("Original: {}", greeting);
    println!("Lowercase: {}", lower_greeting);
}
```
Output:
```
Original: नमस्ते, दुनिया!
Lowercase: नमस्ते, दुनिया!
```
ध्यान दें कि हिंदी चरित्र पहले से ही एक निचले-केस फॉर्म में हैं, तो परिवर्तन अदृश्य होगा। अंग्रेजी उदाहरण के लिए:

```Rust
fn main() {
    let english_greeting = "Hello, World!";
    let lower_english_greeting = english_greeting.to_lowercase();
    println!("Original: {}", english_greeting);
    println!("Lowercase: {}", lower_english_greeting);
}
```
Output:
```
Original: Hello, World!
Lowercase: hello, world!
```

## Deep Dive (गहराई से जानकारी)
Rust की `String` type में `.to_lowercase()` मेथड UTF-8 टेक्स्ट के साथ अच्छे से काम करता है। ये Unicode के case mappings का उपयोग करता है, जिसे इंटरनेशनल कम्युनिटी ने मान्यता दी है।

`.to_lowercase()` दुनियाभर की भाषाओं में विस्तार से लागू होता है, जिसमें कई special case conditions शामिल हैं। 

इसके alternatives में `to_ascii_lowercase()` भी है, जो केवल ASCII चरित्र के लिए काम करता है। यदि आपके text में केवल ASCII चरित्र हों, तो यह तेज़ और सरल समाधान हो सकता है।

Rust का `.to_lowercase()` उच्च performance और safety के साथ डिजाइन किया गया है। यह हमेशा एक नई `String` object बनाता है क्योंकि Rust में strings immutable होते हैं।

## See Also (और जानकारी के लिए)
- Rust के official documentation में `.to_lowercase()` पर [डिटेल जानकारी](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)।
- [Unicode case mappings](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt) के बारे में और जानें।
- Rust by Example पर एक [tutorial](https://doc.rust-lang.org/stable/rust-by-example/std/str.html) है जो strings के अन्य operations को समझाती है।
- [`Ownership`](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html) और [`Borrowing`](https://doc.rust-lang.org/book/ch04-02-references-and-borrowing.html) के सिद्धांत Rust में memory safety की चाबी हैं, जो `.to_lowercase()` जैसे operations को संभव बनाते हैं।
