---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Rust: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत की तारीख की गणना, कार्यक्रम में निश्चित समयावधि के बाद या पहले की तारीख निर्धारित करने की क्रिया है। प्रोग्रामर्स इसे विशेष समय सीमाओं (जैसे की सत्र समाप्ति) और अनुसूचियों की स्थापना जैसी आवश्यकताओं को पूरा करने के लिए करते हैं।

## कैसे:

हम रस्ट के `chrono` लाइब्ररी का उपयोग करके यह कर सकते हैं। उदाहरण के लिए:

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();

    println!("Now: {}", now);

    let two_weeks_from_now = now + Duration::weeks(2);

    println!("Two weeks from now: {}", two_weeks_from_now);
}
```

यह कोड `Duration::weeks(2)` का उपयोग करके वर्तमान समय से दो सप्ताह बाद की तारीख निर्धारित करेगा।

## गहन अध्ययन:

**ऐतिहासिक संदर्भ:** 
समय की गणना करने के लिए पहले `time` लाइब्ररी का उपयोग किया जाता था, लेकिन यह `chrono` की तुलना में कम लचीली और कठिन है।

**वैकल्पिक:**
रस्ट में, आप `time2` जैसे अन्य लाइब्ररी का उपयोग भी कर सकते हैं, जो `chrono` द्वारा प्रदान की जाने वाली क्षमताओं का विस्तार करती है।

**कार्यान्वयन विवरण:**
`Duration` प्रकार का उपयोग करने से, हम सप्ताह, दिन, घंटे, मिनट और सेकंड आदि की इकाईयों में समयावधि को निर्धारित कर सकते हैं। इसके बाद, हम इस `Duration` को वर्तमान `DateTime` पर जोड़ सकते हैं या घटा सकते हैं।

## देखें भी:

1. और अधिक जानकारी के लिए [Rust डेटाइम गणना](https://docs.rs/chrono/0.4.19/chrono/index.html)