---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:38:09.329927-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String se date parse karna matlab ek text format mein likhe gaye date ko Ruby ke `Date` object mein badalna hota hai. Programmers isliye karte hain kyunki data har jagah standard format mein nahi milta aur usse process karne ke liye hume usse samajhne layak format mein convert karna padta hai.

## How to: (कैसे करें:)
```Ruby
require 'date'

# Ek simple string jo date ko represent karta hai
date_string = "2023-03-14"
# String ko parse karke Date object mein convert karna
parsed_date = Date.parse(date_string)
puts parsed_date
# Output: 2023-03-14

# Custom date format ke saath parse karna
custom_date_string = "14-03-2023"
custom_format_date = Date.strptime(custom_date_string, '%d-%m-%Y')
puts custom_format_date
# Output: 2023-03-14
```

## Deep Dive (गहराई से जानकारी):
### Historical Context (ऐतिहासिक संदर्भ)
Ruby mein date parsing ka concept bahut pehle se hai. Ruby 1.9 ke release ke baad se `Date` aur `Time` classes kaafi improve ho chuki hain. Pehle custom parsing thoda tough tha, lekin ab Ruby's built-in `date` library ke `parse` aur `strptime` methods se ye kaafi easy ho gaya hai.

### Alternatives (विकल्प)
Date parsing ke aur bhi methods hain jaise `time` library ka use karke. Wahi `strptime` method `Time` class mein bhi available hota hai, lekin thoda alag context mein.

```Ruby
require 'time'

time_string = "2023-03-14 05:23:17"
parsed_time = Time.strptime(time_string, '%Y-%m-%d %H:%M:%S')
puts parsed_time
# Output: 2023-03-14 05:23:17 +0000
```

### Implementation Details (कार्यान्वयन विवरण)
Ruby ke `Date.parse` method automatically common formats ko pehchanta hai lekin `Date.strptime` ka use karke hum custom formats define kar sakte hain. Ye flexibility tab kaam aati hai jab hum non-standard date formats se deal kar rahe ho.

## See Also (यह भी देखें):
- Ruby's official Date class documentation: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- Ruby's Time class documentation: [https://ruby-doc.org/core-3.0.0/Time.html](https://ruby-doc.org/core-3.0.0/Time.html)
- Ruby's official programming tutorials: [https://www.ruby-lang.org/en/documentation/](https://www.ruby-lang.org/en/documentation/)
- Stack Overflow, for any specific questions about date parsing in Ruby: [https://stackoverflow.com/questions/tagged/ruby+date](https://stackoverflow.com/questions/tagged/ruby+date)
