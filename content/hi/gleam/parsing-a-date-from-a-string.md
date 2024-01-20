---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक तारिक को स्ट्रिंग से पार्स करना, मूल रूप से एक चरित्र श्रृंखला (जैसे "2023/04/15") को कार्यान्वयन योग्य तारीख में बदलने का प्रक्रिया होता है। प्रोग्रामर्स इसे डेटा को और अच्छी तरह से प्रबंधित करने, और उपयोगकर्ता सहुलियत के लिए करते हैं।

## कैसे करें:

आप Gleam में इसे इस तरह से कर सकते हैं:

```Gleam
import gleam/uri

fn parse_date() {
  let date_str = "2023/04/15"
  let result = uri.parse(date_str)

  case result {
     Ok(timestamp) -> io.println(timestamp)
     Error(_) -> io.println("Could not parse date")
  }
}
```

## गहरी डाइव:

तारीख को स्ट्रिंग से पार्स करने का विचार सोफ्टवेयर में काफी समय से मौजूद है। यह श्रृंखलाओं में महत्वपूर्ण तारीख डेटा को संग्रहित और पुनर्प्राप्त करने के लिए एक आसान तरीका प्रदान करता है। 

एक विकल्प हो सकता है की हम तारीख को unix timestamp की तरह संग्रहित कर सकें, लेकिन यह पढ़ने में कठिन होता है। 

Gleam में, यह एक `Uri.parse` कॉल से किया जाता है जो दिए गए स्ट्रिंग को `Ok(Timestamp)` या `Error(_)` में पार्स करता है, जिसके आधार पर एक क्रिया निष्पादित की जाती है।

## अन्य उपयोगी स्रोत:

2. [Parsing in Functional Programming](http://book.realworldhaskell.org/read/using-parsec.html)
3. [DateTime Libraries](https://hex.pm/packages?search=datetime&sort=downloads)
4. [Gleam Date and Time package](https://github.com/gleam-experiments/time)