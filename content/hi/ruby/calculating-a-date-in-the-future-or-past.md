---
title:                "Ruby: भविष्य या भूतकाल में एक तिथि की गणना करना"
simple_title:         "भविष्य या भूतकाल में एक तिथि की गणना करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
गणना को भविष्य या भूतकाल में दिनांक का निर्धारण किया जाता है, जो आवश्यक इंटरफेस, दक्षता और सिद्धांत का आवश्यकता होता है।

## कैसे
```Ruby
# आने वाले 30 दिन के लिए तारीख की गणना
date = Date.today + 30
puts date
=> 2021-08-24

# भूतकाल के 30 दिन पहले के लिए तारीख की गणना
date = Date.today - 30
puts date
=> 2021-06-25
```

## गहराई में गुंजाइश
तारीख को भविष्य या भूतकाल में गणना करने के लिए, हम गहराई से समझने की जरूरत है कि तारीख को कैसे प्रदर्शित किया जाता है और इसके पीछे के सिद्धांतों को कैसे संवालित किया जाता है। इसमें दक्ष बनना आवश्यक है जिससे हम सही तारीख का निर्धारण कर सकें।

## देखो तो
* [क्यों और कैसे आगामी तारीख की गणना?](https://www.learnrubyonline.org/en/lessons/date_and_time)
* [भूतकाल में तारीख की गणना कैसे करें?](https://www.rubyguides.com/2019/01/ruby-time-and-date/)
* [तारीख और समय को Ruby में कैसे उपयोग करें?](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)

शुभकामनाएं! आशा है कि यह आरंभिक मार्गदर्शन आपको आगामी तारीख की गणना करने में मदद करेगा। रूबी से संबंधित और भी रोचक सामग्री के लिए "निकट भविष्य में देखो" जरूर देखें!