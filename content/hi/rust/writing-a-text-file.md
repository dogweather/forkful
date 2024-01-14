---
title:                "Rust: टेक्स्ट फ़ाइल लिखना"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट फाइल लिखने में क्यों लगना चाहिए? टेक्स्ट फाइलें किसी भी प्रोग्रामिंग लैंग्वेज में कोड लिखने का सबसे सरल और आसान तरीका है। इसलिए, हिंदी में रस्ट प्रोग्रामिंग ब्लॉग पोस्ट के माध्यम से हम टेक्स्ट फाइल लिखने के बारे में जानेंगे।

## कैसे

```Rust
fn main() {
    let file_contents = "यह हिंदी में एक टेक्स्ट फाइल है।";
    std::fs::write("example.txt", file_contents).expect("फ़ाइल लिखने में त्रुटि।");
}
```

ऊपर दिए गए कोड विभिन्न भाषाओं में हार्डकोडेड स्ट्रिंग को टेक्स्ट फाइल में लिखने का उदाहरण है। आप अपने आवश्यकतानुसार कोड में पट्टियों, अक्षर या अन्य डेटा को शामिल कर सकते हैं। `write` फ़ंक्शन फ़ाइल नाम और फ़ाइल में लिखने का डेटा प्रदान करने के लिए प्रोटोटाइप है।

## डीप डाइव

टेक्स्ट फाइल लिखने का यह तरीका केवल bokohaim फ़ंक्शन में उपलब्ध है। इसके अलावा, हम `std::fs::write` के साथ अन्य फंक्शन का भी उपयोग कर सकते हैं। इसे अधिक विशेष करने के लिए [रस्ट दिलचस्प सीखना: फाइलें](https://rust-lang-nursery.github.io/rust-cookbook/file.html) पर जाएं।

## देखिये भी

- [रस्ट दिलचस्प सीखना: फाइलें](https://rust-lang-nursery.github.io/rust-cookbook/file.html)
- [हिंदी में रस्ट प्रोग्रामिंग सीखें](https://github.com/Jaikishan-PM/Rust-Learning-in-Hindi)
- [रस्ट भाषा के आधिकारिक वेबसाइट](https://www.rust-lang.org/)