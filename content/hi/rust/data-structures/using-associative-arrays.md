---
title:                "सहयोगी अरेज़ का उपयोग करना"
aliases:
- /hi/rust/using-associative-arrays/
date:                  2024-01-30T19:13:29.576559-07:00
model:                 gpt-4-0125-preview
simple_title:         "सहयोगी अरेज़ का उपयोग करना"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

संबद्ध एरे, या जैसा कि Rustaceans "हैश मैप्स" कहते हैं, ऐसे संग्रह होते हैं जो डेटा को कुंजी-मूल्य जोड़ों में संग्रहित करते हैं। प्रोग्रामर्स उनका उपयोग त्वरित डेटा लुकअप के लिए करते हैं, जिससे अनूठी कुंजियों के आधार पर डेटा का कुशलतापूर्वक संशोधन संभव होता है।

## कैसे:

Rust में, `std::collections` मॉड्यूल से `HashMap` प्रकार संबद्ध एरे की कार्यक्षमता प्रदान करता है। यहाँ आप उनके साथ काम कैसे कर सकते हैं:

```Rust
use std::collections::HashMap;

fn main() {
    // एक नया HashMap बनाना
    let mut scores = HashMap::new();

    // मूल्य डालना
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // मूल्यों का प्राप्त करना
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Blue टीम के लिए स्कोर: {}", score); // परिणाम: Blue टीम के लिए स्कोर: 10
    }

    // एक मूल्य को अपडेट करना
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // कुंजी-मूल्य जोड़ों पर इटरेट करना
    for (key, value) in &scores {
        println!("{}: {}", key, value); // परिणाम: Blue: 15, Yellow: 50
    }
}
```

## गहराई में जानकारी

Rust का `HashMap` कुंजियों को मूल्यों से जोड़ने के लिए एक हैशिंग फंक्शन का उपयोग करता है, जो तेज़ी से डेटा पुनः प्राप्ति को सक्षम बनाता है। हालाँकि, यह कार्यक्षमता एक लागत के साथ आती है: हैश मैप्स उनके तत्वों के क्रम को बरकरार नहीं रखते हैं। यह पायथन (`dict`) या रूबी जैसी अन्य संबद्ध एरे कार्यान्वयनों के विपरीत है, जो हाल के संस्करणों में एक विशेषता के रूप में सम्मिलन क्रम बनाए रखती हैं। ऐसे मामलों में जहाँ कुंजी-मूल्य जोड़ों का क्रम महत्वपूर्ण होता है, Rust डेवलपर्स `std::collections` मॉड्यूल से `BTreeMap` का उपयोग करने पर विचार कर सकते हैं, जो क्रम को बनाए रखता है लेकिन `HashMap` की तुलना में धीमे सम्मिलन और पुनः प्राप्ति प्रदान कर सकता है। अंततः, `HashMap` और `BTreeMap` के बीच चयन विशेष आवश्यकताओं पर निर्भर करता है जो क्रम और प्रदर्शन से संबंधित होती हैं।
