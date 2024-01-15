---
title:                "तारीख को स्ट्रिंग में रूपांतरित करना"
html_title:           "Rust: तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

Date को string में convert करना एक आम टास्क है जो कई अलग-अलग उद्देश्यों के लिए किया जाता है। इसको automate करने से आपको समय, कष्ट और गलतियां बचाने में मदद मिल सकती है।

## कैसे करें

कैसे आप अपने Rust प्रोजेक्ट में Date को string में convert कर सकते हैं? इसके लिए हमारे पास कई विकल्प हैं। आप मूल्य तर्क या किसी अन्य प्रोग्रामिंग भाषा के साथ सहयोग कर सकते हैं, लेकिन हम आपको आसान और उपयोगशील तरीकों के बारे में बताएंगे।

```Rust
fn main() {
    use chrono::{DateTime, Utc};
    
    // Getting current datetime
    let now: DateTime<Utc> = Utc::now();

    // Converting to string
    let date_string = now.format("%Y-%m-%d %H:%M:%S").to_string();

    println!("{}", date_string); // Output: 2021-07-12 14:30:00
}
```

उपरोक्त उदाहरण में, हमने Rust की `format()` फ़ंक्शन का उपयोग करके Date को string में convert किया है। आप `%Y`, `%m`, `%d` आदि के द्वारा दिनांक, महीने और वर्ष का प्रारूप निर्धारित कर सकते हैं। आप अपनी आवश्यकतानुसार निर्धारित किया गया प्रारूप उपयोग कर सकते हैं।

## गहराई में

डेटा को string में convert करने का यह तरीका date-time के साथ मज़बूत रूप से सामग्रीकरण करता है। इस लेख के अन्य भागों में हम आपको अन्य विकल्पों के बारे में बताएंगे जो आपको इस काम को करने में मदद कर सकते हैं। Rust के अलावा, आप date-time को string में convert करने के लिए सामान्य रूप से अन्य पॉपुलर प्रोग्रामिंग भाषाओं जैसे PHP, Java, और Python का उपयोग कर सकते हैं।

## देखें भी

- [Rust गाइ