---
title:    "Ruby: तिथि को स्ट्रिंग में रूपांतरित करना"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Kyun: 
Date ko string mein convert karne ka ek sabse aam karan hai ki hum isko humare code mein display karna chahte hai. Yeh humare liye date-time formatting mein kaafi aasan aur flexible tarike hai.

## Kaise Karein:
```
Ruby aasan tarike se date ko string mein convert karne ki permission deta hai. Kuch simple examples neeche diye gaye hain:

### Convert current date into string:
```
Time.now.strftime("%d-%m-%Y")
=> "23-07-2021"

### Convert specific date into string:
```
Date.new(2021, 10, 25).strftime("%Y-%m-%d")
=> "2021-10-25"

### Convert date with time into string:
```
DateTime.now.strftime("%d/%m/%Y %H:%M:%S")
=> "23/07/2021 14:36:25"

## Deep Dive:
Ruby mein date ko string mein convert karne ke liye `strftime` method ka use kiya jaata hai. Yeh method date-time ke specific components ka use karke hume required output format generate karta hai. Iske liye hum `%` character ke baad format specifier ka use karte hai. Kuch common format specifier hain:
- `%Y` - Year (YYYY) 
- `%m` - Month (MM)
- `%d` - Day of the month (DD)
- `%H` - Hour (24-hour format)
- `%M` - Minute
- `%S` - Second 

## Dekhiye bhi:
Inn links se aap aur bhi detail mein date ko string mein convert karne ke methods aur options explore kar sakte hain:
- https://www.rubyguides.com/2015/10/ruby-strftime/
- https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html#method-i-strftime
- https://www.freecodecamp.org/news/easily-format-date-and-time-in-ruby-79a79ebffa0/ 

Yeh tha date ko string mein convert karne ke baare mein ek chota sa tutorial. Agar aapko aur kisi topic pe Ruby programming se related koi post chahiye, toh humein zaroor bataiye! Happy coding!