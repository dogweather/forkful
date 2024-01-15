---
title:                "भविष्य या भूतकाल में एक तारीख की गणना"
html_title:           "Ruby: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyun
Agar aap ek Ruby programmer hai, toh aapne shayad kabhi na kabhi future ya past mein kisi specific date ko calculate kiya hoga. Ye kaam bohot common hota hai aur hum iska use different applications mein karte hai. Is article mein hum aapko bataenge ki kaise aap Ruby mein aasani se date ko calculate kar sakte hai.

## Kaise
Code mein ek date ko calculate karna kaafi simple hai. Hum bas `DateTime` class ke `future` ya `past` method ka use karenge aur usme desired number of days ya years add ya subtract kar denge. Neeche diye gaye code block ko dekhein:

```Ruby
require 'date'

# Current date
current_date = DateTime.now

# Calculate future date
future_date = current_date.future(21) # Calculates future date 21 days from now
puts future_date.strftime("%d %b %Y") # Output: 15 Jul 2020

# Calculate past date
past_date = current_date.past(10) # Calculates past date 10 days before current date
puts past_date.strftime("%d %b %Y") # Output: 25 Jun 2020
```

## Deep Dive
Ruby mein date ko calculate karne ke liye hum DateTime class ka use karte hai. Ismein kuch lambai aur chaudai add ya subtract karne ke methods hai jaise `future`, `past`, `next_day`, `prev_day`, etc. Iske alawa, hum `days` aur `years` method bhi use kar sakte hai jis se hum precise calculations kar sakte hai. Ye sab methods aapko date ko manipulate karne mein madad karenge.

## Dekhiye
Agar aapko date calculate karne ke alawa bhi Ruby programming se related articles padhne hai, toh aap neeche diye gaye links ko check kar sakte hai:
- [Introduction to Ruby Programming](https://www.geeksforgeeks.org/ruby-programming-language/)
- [Manipulating Dates and Times in Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-ruby)
- [Ruby DateTime Class](https://www.rubyguides.com/2015/12/ruby-datetime/)