---
title:                "भविष्य या भूतकाल में तारीख की गणना"
html_title:           "Rust: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Date की गणना क्या है और क्यों करते हैं?

Date की गणना एक तिथि या समय की गणना है, जो पहले या बाद में एक निश्चित मात्रा में बदलने के लिए किया जाता है। प्रोग्रामर्स तिथियाँ गणना करते हैं ताकि वे अपने कोड में समय संबंधी कार्यों को अधिक सुविधाजनक बना सकें।

## कैसे करें:

```
Rust fn calculate_date(month: u32, day: u32) {
    println!("Month: {}, Day: {}", month, day);
}

fn main() {
    // calculate date for July 12
    calculate_date(7, 12);
}
```

आउटपुट:
Month: 7, Day: 12

## गहराई में जाएं:

तिथियाँ गणना करने के लिए कई विभिन्न तरीके हैं, जैसे डेट टाइम (Django), chrono (Rust), आदि। अलग-अलग भाषाओं जैसे Java, Python भी इसे समर्थित करते हैं। इसके अलावा, आप अपने जगह के समय क्षेत्र को ध्यान में रखकर तारीखों को गणना कर सकते हैं।

## इसी तरह देखें:

अधिक जानकारी के लिए, आप निम्न लिंकों को देख सकते हैं:
- [डेट टाइम (Django)]: https://docs.djangoproject.com/en/3.2/ref/templates/builtins/#date
- [chrono (Rust)]: https://docs.rs/chrono/0.4.19/chrono/index.html
- [Java]: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- [Python]: https://docs.python.org/3/library/datetime.html