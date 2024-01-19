---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# रूबी में दो तारीखों की तुलना करना: एक मार्गदर्शिका 

## क्या और क्यों?

तारीखों की तुलना एक बहुत ही सामान्य कार्य होता है. यहां हम दो तारीखों को तुलना करना सीखेंगे, जिससे हमें समयावधि, तारीख-वक्त की गणनाें और तारीख-यातायात में सहायता मिल सकती है।

## कैसे करें:

रूबी में दो तारीखों की तुलना करना बहुत सरल है। ये कोड उदाहरण देखिए:
```Ruby
require 'date'

date1 = Date.new(2021, 05, 15)
date2 = Date.new(2021, 06, 01)

if date1 > date2
  puts "date1 is later"
elsif date1 < date2
  puts "date2 is later"
else
  puts "both dates are the same"
end
```
यदि हम इस कोड को चलाते हैं, हमें `date2 is later` का परिणाम मिलेगा, क्योंकि 01 जून 2021, 15 मई 2021 से बाद का दिन है।

##गहरी जांच:

रूबी भाषा में `Date` लाइब्रेरी का इस्तेमाल करके हम तारीखों की तुलना कर सकते हैं। इसके अलावा, आप `Time` वर्ग का भी इस्तेमाल कर सकते हैं, जब आपको विशेष समय पर ध्यान केंद्रित करना हो।
वैकल्पिक रूप से, आप `DateTime` क्लास का उपयोग कर सकते हैं, जो मिलिसेकंड तक की गणना सटीकता से कर सकता है।

## आगे देखें:

- दो तारीखों के बीच के दिनों की गणना कैसे करें?: [Link](https://stackoverflow.com/questions/4502245/how-can-i-calculate-the-number-of-days-between-two-date-objects-in-ruby)
- एक तारीख को दूसरी तारीख में परिवर्तित कैसे करें?: [Link](https://stackoverflow.com/questions/5937577/convert-date-to-another-time-zone-in-ruby)
- कुछ विशेष तारीखों की तुलना कैसे करें?: [Link](https://stackoverflow.com/questions/38387529/comparing-some-specific-dates-in-ruby)