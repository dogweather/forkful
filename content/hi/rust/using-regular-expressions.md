---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Regular expressions, जिन्हें regex भी कहते हैं, टेक्स्ट को मैच, ढूंढ और मैनिप्युलेट करने के लिए प्रयोग किए जाने वाले पैटर्न होते हैं। Programmers इनका इस्तेमाल डेटा वैलिडेशन, पार्सिंग और ट्रांसफॉर्मेशन के लिए करते हैं।

## How to: (कैसे करें:)
```Rust
// regex crate को शामिल करें
use regex::Regex;

fn main() {
    // Email regex पैटर्न बनाएं
    let email_re = Regex::new(r"^\w+@\w+\.\w+$").unwrap();
    
    // टेक्स्ट की जांच करें
    let is_valid = email_re.is_match("someone@example.com");
    
    // परिणाम छापें
    println!("Is the email valid? {}", is_valid);
}

// सैंपल आउटपुट:
// Is the email valid? true
```

## Deep Dive (गहन जानकारी)
Regular expressions की शुरुआत 1950 के दशक में हुई और आज के अधिकांश programming languages में इस्तेमाल होते हैं। Alternatives के रूप में String searching algorithms जैसे KMP (Knuth-Morris-Pratt) हैं, लेकिन regex अधिक वर्सेटाइल हैं। Rust में Regex library efficient और सुरक्षित ढंग से implemented है, जिसमें compile-time और runtime errors का बहुत ही कम चांस होता है।

## See Also (अधिक जानकारी के लिए)
- Rust `regex` crate documentation: [docs.rs/regex](https://docs.rs/regex/)
- Regular Expressions को गहराई से सीखने के लिए: [regexone.com](https://regexone.com/)
- Regular Expression के टेस्ट/डिबग के लिए: [regex101.com](https://regex101.com/)
