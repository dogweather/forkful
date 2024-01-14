---
title:                "Gleam: भविष्य या अतीत में एक तिथि की गणना"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

जब हमारे पास डिब्बों और उदयपन्थियों संबंधित दस्तावेज या शिशु खेलने की अनुमति नहीं हो, हम रुचि से ऐसे कारण के लिए गणना करने में होते हैं जो कुछ समय बाद होगा या उससे पूर्व.

## कैसे

```Gleam
import Gleam.Date

let future_date = Date.add_days(Date.today(), 7)
let past_date = Date.sub_days(Date.today(), 7)

IO.println("आज से 7 दिन बाद की तारीख: #{future_date}")
IO.println("आज से 7 दिन पहले की तारीख: #{past_date}")
```

## गहराई में जाने

गणना करने का एक सबसे आसान तरीका यह है कि दिनांक के साथ दिनों को जोड़ना और घटाना। इसके अलावा, आप अधिक गहराई में जानने के लिए Gleam की कमानाएं और जैविक गणनाएं इस्तेमाल कर सकते हैं।

## देखें भी

- [Gleam तिथि खेल](https://gleam.run/playground/date)
- [Gleam कार्य  
](https://gleam.run/docs/getting-started/working-with-dates)