---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
नया प्रोजेक्ट शुरू करना का मतलब होता है स्वतंत्र स्रोत कोड का एक नया सेट तैयार करना, जो एक विशिष्ट कार्य को पूरा करता है। कार्यक्रमकर्ता इसे नई समस्याओं को हल करने के लिए और अधिक कुशल तरीके से करते हैं।

## कैसे करें:
Rust में नया प्रोजेक्ट शुरू करने के लिए अपने टर्मिनल में निम्नलिखित कोड चलाएं:

```Rust
$ cargo new my_project
$ cd my_project
```

यह एक नया डायरेक्टरी `my_project` बनाएगा, जिसमें `Cargo.toml` (Rust के पैकेज मैनेजर के लिए एक अत्यावश्यक फ़ाइल) और `src` डायरेक्टरी मौजूद होता है। हाय, आपका Rust प्रोजेक्ट तैयार है!

## गहरी जाँच:
Cargo, Rust का पैकेज मैनेजर, 2015 में Rust के 1.0 वर्जन के साथ आया था। इससे पहले, Rust प्रोजेक्ट को शुरू करने का कोई एकीकृत तरीका नहीं था। Cargo के विकल्पों में Python का Pip और JavaScript का NPM शामिल हैं, लेकिन Cargo Rust के स्पेसिफिक उपयोग को देखते हुए तैयार किया गया है।

## देखें भी:
- [Rust का आधिकारिक प्रलेखन](https://doc.rust-lang.org/stable/book/)
- [Cargo का प्रलेखन](https://doc.rust-lang.org/cargo/)
- [Cargo गाइड](https://doc.rust-lang.org/cargo/guide/)