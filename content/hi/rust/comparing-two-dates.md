---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Rust में दो तारीखों की तुलना (वर्तमान संस्करण)
इस लेख में हम प्रोग्रामिंग भाषा Rust में दो तारीखों की तुलना के बारे में चर्चा करेंगे।  

## क्या और क्यों?
तारीख की तुलना यानी दो तारीखों को आपस में तुलना करना। प्रोग्रामर्स इसे तब करते हैं जब उन्हें दो घटनाओं के बीच का समय निर्धारित करना होता है। 

## कैसे करें:
यहाँ दो तारीखों की Rust में तुलना की कोडिंग उदाहरण दिया गया है: 

```Rust
use std::cmp::Ordering;
use chrono::NaiveDate;

fn main() {
    let date1 = NaiveDate::from_ymd(2021, 7, 5);
    let date2 = NaiveDate::from_ymd(2022, 7, 5);

    match date1.cmp(&date2) {
        Ordering::Less => println!("तारीख 1 पहले है"),
        Ordering::Equal => println!("तारीखें बराबर है"),
        Ordering::Greater => println!("तारीख 2 पहले है"),
    }
}
```

आउटपुट:
```
तारीख 1 पहले है
```

## गहरी जानकारी
1) ऐतिहासिक प्रकरण: कम्प्यूटर प्रोग्रामिंग में तारीखों की तुलना करने की क्षमता सदियों से ही रही है। Rust भाषा ने समय और तारीख की कामकाजी संगठन के लिए `chrono` क्रेट का उपयोग करके इसे और सुगम बनाया है।
2) विकल्प: आपके पास Rust में तारीखों की तुलना करने के लिए कई विकल्प हैं, जैसे `time` और `date` क्रेटस। 
3) कार्यान्वयन विस्तार: Chrono में `cmp()` फ़ंक्शन आपको Ordering enum के आधार पर तारीखों की तुलना करने की सुविधा देता है (Less, Equal, Greater)।

## देखना भी:
1) [Chrono Crates Documentation](https://docs.rs/chrono/0.4.19/chrono/)
2) [Rust Official Documentation](https://doc.rust-lang.org/rust-by-example/std/chrono.html)
3) [Stack Overflow Community](https://stackoverflow.com/questions/30186520/how-to-compare-dates-in-rust)