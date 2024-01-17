---
title:                "दो तारीखों का तुलना करना"
html_title:           "Ruby: दो तारीखों का तुलना करना"
simple_title:         "दो तारीखों का तुलना करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# क्या और क्यों?
दो तारीखों को तुलना करना क्या है और इसको क्यों प्रोग्रामर्स करते हैं? दो तारीखों को तुलना करना एक आम कार्य है जो कोडिंग में अहम भूमिका निभाता है। यह प्रोग्रामर्स को अपने कोड को सुनिश्चित करने में मदद करता है कि दो तारीखों के बीच अंतर समझने और लंबाई के आधार पर विभिन्न कार्यों का निर्धारण करने में।

# कैसे:
```Ruby
# दो तारीखों के बीच अंतर निकालें
first_date = Date.new(2021, 10, 5)
second_date = Date.new(2021, 10, 8)
difference = second_date - first_date
puts difference # 3

# दो तारीखों के बीच तुलना करें
first_date = Date.new(2021, 10, 5)
second_date = Date.new(2021, 10, 8)
if first_date < second_date
  puts "पहली तारीख दूसरी तारीख से पहले है"
else
  puts "पहली तारीख दूसरी तारीख से बाद है"
end
```

# गहराई में जाएं:
दो तारीखों को तुलना करने के लिए कई विभिन्न तरीके हैं, जैसे date और time स्ट्रक्चर्स का उपयोग करना या स्ट्रिंग या नंबर को तारीख में प्रेरित करना। इन सभी तकनीकों में, date और time स्ट्रक्चर्स सबसे आसान और समझने में सरल हैं। इनका उपयोग करके प्रोग्रामर्स तारीखों को तुलना कर सकते हैं और दो तारीखों के बीच अंतर को निकाल सकते हैं।

# और देखें:
प्रोग्रामिंग में दो तारीखों को तुलना करने के और तरीकों को जानने के लिए निम्न लिंक की जाँच करें:

- [Date and Time in Ruby](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Using date arithmetic in Ruby](https://www.rubyguides.com/2015/07/ruby-date/)
- [Date Comparison in Ruby](https://www.geeksforgeeks.org/comparing-dates-in-ruby/)