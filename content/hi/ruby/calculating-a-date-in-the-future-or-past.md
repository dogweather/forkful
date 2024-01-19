---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Ruby: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य या अतीत की तारीख की गणना करना मतलब किसी विशेष तारीख से अगले या पिछले कुछ समय की तारीख को ज्ञात करना। कार्यक्रमकर्ताओं को इसे इसलिए करना पड़ता है क्योंकि यह तारीख पर आधारित ऍप्लिकेशन्स निर्माण करने में मदद करता है। 

## कैसे करें:
Ruby में `Date` क्लास का उपयोग करके आप तारीखों की गणना कर सकते हैं। 

```Ruby
require 'date'

# आज की तारीख
today = Date.today
puts "आज की तारीख: #{today}"

# 5 दिन बाद की तारीख
future_date = today + 5
puts "5 दिन बाद की तारीख: #{future_date}"

# 7 दिन पहले की तारीख
past_date = today - 7
puts "7 दिन पहले की तारीख: #{past_date}"
```
तारीख की गणना करने के लिए '+' और '-' ऑपरेटर का उपयोग किया जा रहा है। 

## गहरा आवेशन
Ruby में `Date` क्लास का परिचय रूबी 1.9.2 के साथ हुआ जिसने तारीख की संगणना करने की प्रक्रिया को बहुत सरल बना दिया। इसके विकल्प में `Time` क्लास और गहरा 'DateTime' क्लास मिलते हैं, लेकिन `Date` क्लास की सरलता और स्पष्टता को मानते हुए यह सबसे अधिक प्रेरित करने वाला है।

## अधिक देखें
1. [Ruby Docs - Date Class](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html) - रूबी Date क्लास की विस्तृत जानकारी के लिए। 
2. [Ruby Guides - Date & Time in Ruby](https://www.rubyguides.com/2015/12/ruby-time/) - Ruby में तारीख और समय के बारे में अधिक जानकारी के लिए।
3. [StackOverflow - Calculating Future/Past Dates](https://stackoverflow.com/questions/2737949/ruby-see-if-a-date-is-in-the-past-or-future) - Ruby में भविष्य/अतीत की तारीख की गणना के उदाहरण के लिए।