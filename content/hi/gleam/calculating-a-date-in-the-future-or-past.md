---
title:                "भविष्य या भूतकाल में एक तारीख की गणना"
html_title:           "Gleam: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे पास एक तारीख की संज्ञा नहीं होती है और हमें उसे आगे या पीछे के संबंध में जानने की जरूरत हो सकती है। इसलिए, Gleam कोडिंग में आगे की तारीख का गणना करना या पीछे की तारीख का गणना करना बहुत महत्वपूर्ण है।

## कैसे करें

आप Gleam में आगे या पीछे की तारीख का गणना करने के लिए `Date.arithmetic` फंक्शन का प्रयोग कर सकते हैं। नीचे दिए गए उदाहरण में, हम 10 दिनों के आगे की तारीख को गणना करेंगे:

```Gleam
import Date

let today = Date.now()
let future_date = Date.arithmetic(today, Date.Duration(days: 10))

Debug.print("Future Date:", future_date)
```

आउटपुट:

```
Future Date: 2021-06-11T00:00:00Z
```

इसी तरह, आप पीछे की तारीख का भी गणना कर सकते हैं, बस फंक्शन को उम्राज्ञान आदेश (`ADate.Return`) के साथ बुलाना होगा। उदाहरण के लिए:

```Gleam
import Date

let today = Date.now()
let past_date = Date.arithmetic(today, Date.Duration(days: -5), ADate.Return)

Debug.print("Past Date:", past_date)
```

आउटपुट:

```
Past Date: 2021-06-01T00:00:00Z
```

## गहरी जांच

आप Gleam में आगे या पीछे की तारीख का गणना करते समय `Date.Duration` अपवर्तक सेट आदेश का उपयोग कर सकते हैं। इस अपवर्तक सेट आदेश से आप सभी प्रकार के संख्यात्मक स्वीकृत मूल्य का उपयोग कर सकते हैं, जैसे: वर्षों (`years`), महीनों (`months`), सप्ताह (`weeks`) आदि। जितना आपको चाहिए।

जो कुछ भी आप चुनें, `Duration` अपवर्तक सेट आदेश को आप उस पर `arithmetic` फंक्शन के साथ इस्तेमाल कर सकते हैं और जो भी तारीख आपन