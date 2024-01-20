---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दो तारीखों की तुलना करने से मतलब है, एक तारीख को दूसरी तारीख से तुलना करना। प्रोग्रामर इसे करते हैं ताकि वे तारीखों के बीच के समय की अवधि को गणना कर सकें और टाइम-बेस्ड लॉजिक को समझ सकें।

## कैसे करें

Gleam कोड उदाहरण के साथ, दो तारीखों की तुलना करने का तरीका निम्नलिखित है:

```Gleam
import gleam/stdlib/dates
import gleam/stdlib/int
import gleam/ok.{Ok, Error}

fn main() {
    let date1 = dates.new(2021, 12, 25)
    let date2 = dates.new(2023, 1, 1)

    match (date1, date2) {
        (Ok(d1), Ok(d2)) => {
            let diff = dates.diff_days(d1, d2)
            io.println(diff)
            case diff {
                Ok(days) => io.println("दिनों का अंतर: " ++ int.to_string(days))
                Error(err) => io.println("एरर: " ++ err)
            }
        }
        _ => io.println("तारीख ग़लत है")
    }
}
```

इस कोड के उत्पादन में "दिनों का अंतर: 372" प्रिंट होगा, जिसका अर्थ होता है कि दो तारीखों के बीच 372 दिन का अंतर है।

## गहन जानकारी

तारीखों की तुलना करने वाले कोड के विकास का इतिहास उन तकनीकों और अवधारणाओं से जुड़ा हुआ है जिन्हें प्रोग्रामर्स ने समय और तारीख की गणना के लिए विकसित किया है। इसके विकल्प के रूप में समय मुद्राओं का उपयोग किया जा सकता है, जो UNIX एपोक के सेकेंड में मापे जाते हैं। Gleam में इसका कार्यान्वयन `gleam/dates` पैकेज में होता है, यह एक्स्कर्सन के टाइम लाइब्ररी को अंतर्निहित करता है।

## आगे देखें

- Gleam कार्यालय की [दस्तावेज़ीकरण](https://hexdocs.pm/gleam_stdlib/gleam/dates.html)
- [Gleam कोडिंग टुटोरियल](https://gleam.run/book/tour/integers.html)
- [UNIX एपोक के बारे में विकिपीडिया लेख](https://en.wikipedia.org/wiki/Unix_time)