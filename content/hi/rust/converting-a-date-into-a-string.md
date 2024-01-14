---
title:                "Rust: तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

वस्तुत: रास्ट प्रोग्रामिंग भाषा में तारीख को स्ट्रिंग में रूपांतरित करना असाध्य नहीं है। आप एक आसान तरीके से तारीख को स्ट्रिंग में परिवर्तित कर सकते हैं।

कैसे: कोडिंग उदाहरण और "```Rust ... ```" कोड ब्लॉक में उदाहरण आउटपुट।

```Rust
use chrono::{Datelike, TimeZone, Utc};
​
//अगर आप इस सन्दर्भ में अक्सर तारीख को स्ट्रिंग में बदलते हैं, तो आपको अब इसके लिए फ़ंक्शन रेटिंग के बारे में जाननी चाहिए।
fn date_to_string(date: chrono::DateTime<Utc>) -> String {
   let year = date.year();
   //महीना 1 अंतरित करने के लिए उपयोग में जाने के लिए
year = 12 महीनों का एक्सेंभिनल
   let month = date.month();
   let day = date.day();
​
// माह में 1 अंतरित करने के लिए उपयोग में जाने के लिए
   let month_str = match month {
       1 => "जनवरी",
       2 => "फरवरी",
       3 => "मार्च",
       // बाकि सब एक प्रकार से लिखा गया है
       // ...
       12 => "दिसम्बर",
       _ => unreachable!(),
   };
​
// उदाहरण
   format!("{} तारीख {} में {}", day, month_str, year)
}

​​fn main() {
   let date = Utc.ymd(2021, 8, 22).and_hms(0, 0, 0);
   let date_string = date_to_string(date);
​
   println!("{}", date_string); // आउटपुट: 22 अगस्त 2021
}
```

गहराई में जाइए: तारीख को स्ट्रिंग में रूपांतरित करने के लिए, आपको पहले रास्ट भाषा और उसमें उपलब्ध चरणों को समझना होगा। आपको समझने की जरूरत होगी कि कैसे प्रोग्रामिंग भाषा तारीखों को संबोधित करती है और आपको चरणों को अनुकूलित करें। आप डेटा और समय तारीखों को कैसे प