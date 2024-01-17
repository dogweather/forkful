---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Swift: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
एक तिथि को भविष्य या अतीत में गणना करना यह है कि हम देखते हैं कि एक निश्चित समय के बाद / पहले की तारीख क्या होगी। प्रोग्रामर्स ऐसा इसलिए करते हैं कि वे अपने प्रोग्राम में इस विशेषता को शामिल कर सकें जो कुछ दिनों, हफ्तों या सालों के बाद / पहले कैसे होगा।

## कैसे करें:
```Swift
// एक निश्चित तारीख कोांडनोंल करें
let date = Date()

// तारीख को एक मास आगे देखना
let futureDate = Calendar.current.date(byAdding: .month, value: 1, to: date)

// तारीख को एक साल पहले देखना
let pastDate = Calendar.current.date(byAdding: .year, value: -1, to: date)

// तारीख को एक हफ्ते आगे देखना
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: date)
```

## गहराई में खोजें:
इतिहासी परिवेश, विकल्प और तारीख को भविष्य या अतीत में गणना करने के तरीके के लिए विस्तृत जानकारी।

## देखें भी:
जुड़े हुए स्रोतों के लिंक।