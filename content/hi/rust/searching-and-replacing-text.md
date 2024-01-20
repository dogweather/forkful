---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

**तलाशना और बदलना** एक पाठ (टेक्स्ट) को खोजना और उसे दूसरे पाठ से बदलना है। प्रोग्रामर यह कार्य करते हैं ताकि वे डेटा को संशोधित कर सकें और आवश्यकतानुसार उसे पुन: प्राप्त कर सकें।

## कैसे करें:

```Rust
fn main() {
    let text = "नमस्ते, दुनिया!";
    let replaced = text.replace("दुनिया", "Rust");
    println!("{}", replaced);
}
```
उदाहरण का परिणाम:
```
नमस्ते, Rust!
```

## गहराई से जानकारी:

1. **ऐतिहासिक प्रसंग:** Text खोजने और बदलने की क्षमता को 1970 के दशक में Unix के विकासकर्ताओं ने पियनियर किया। 

2. **विकल्प:** Rust में, आप `str::find` और `str::insert_str` में चुन सकते है, लेकिन यह अधिक कठिन हो सकता है और तलाशना और बदलना विधि से बदल नहीं पाएगा।

3. **कार्यान्वयन विवरण:** `str::replace` Rust में 'हिप' मेमोरी का उपयोग करता है, जो एक जीवित उपयोग करके उत्पादन को 'तात्कालिक' बनाता है।

## देखिए भी:

- [आधिकारिक Rust दस्तावेज़ीकरण](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rust by Example - तार String](https://doc.rust-lang.org/rust-by-example/std/str.html)