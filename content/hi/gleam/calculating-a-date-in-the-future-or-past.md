---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Gleam: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य या अतीत में किसी तारीख की गणना करना क्या है? यह एक कार्य है जिसमें हम वर्तमान तारीख से निर्धारित समयावधि जोड़ या घटाते हैं हमें अन्य तारीख मिलती है । प्रोग्रामर्स इसे क्यों करते हैं? टाइम-संबंधी कार्यों को सुगम बनाने के उद्देश्य से, जैसे नियमित जीवन के कार्यक्रमों की योजना या सदस्यता सेवाओं का प्रबंधन। 

## कैसे करें:
आवेश्यक Gleam कोड का उदाहरण नीचे देखें:

```Gleam
import gleam/calendar.{Date}
import gleam/time.{Duration}

fn future_date(days: Int) {
  let today = Date.today()
  let future = today.add(Duration::days(days))
  future
}
```
इस कोड का आउटपुट आज की तारीख से निर्धारित दिनों के बाद की तारीख देगा।

## गहरी बातें:
विगत आवधियों में, अतीत या भविष्य की तारीख की गणना करने के लिए विशेष नियम पित्री पद्धतियाँ का उपयोग किया जाता था। लेकिन Gleam जैसी आधुनिक भाषाओं ने यह प्रक्रिया सरल और कुशल बना दी है। एक विकल्प botox-rs लाइब्रेरी है, जिसे Rust में लिखा गया है, लेकिन Gleam की सरलता और अभिव्यक्तिशीलता उसे एक बेहतर विकल्प बनाती है। Gleam बेहद नई भाषा होने के बावजूद, समय और तारीख संबंधी फ़ंक्शनलिटी पूरी तरह से समाहित करती है।

## और भी पढ़ें:
2. Gleam के डॉक्युमेंटेशन के लिए: [Gleam Documentation](https://hexdocs.pm/gleam/)