---
title:                "Parsing a date from a string"
html_title:           "Ruby recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string is the process of extracting and converting a date from a string of text into a format that a computer can understand and manipulate. Programmers do this to make it easier to work with dates in their code, such as comparing dates or displaying them in a specific format.

## How to:
To parse a date from a string in Ruby, we can use the ```DateTime.parse()``` method. This method takes in a string as an argument and returns a DateTime object, which can then be manipulated as needed. Here's an example:

```Ruby
require 'date'

date_string = "2021-05-15" # 15th of May, 2021
date = DateTime.parse(date_string)
puts date # 2021-05-15T00:00:00+00:00
puts date.year # 2021
puts date.month # 5
puts date.day # 15
```

In the code above, we first require the ```date``` library, which provides us with the necessary methods for date manipulation. Then, we create a string with a specific date and pass it into the ```DateTime.parse()``` method. We can then access different components of the date, such as year, month, and day, using the appropriate methods.

## Deep Dive:
Parsing dates from strings has been a common task for programmers since the early days of computing. With the rise of the internet and internationalization, the need for standardized date parsing became more crucial. This led to the creation of ISO 8601, a global standard for date representation, which the ```DateTime.parse()``` method in Ruby adheres to.

An alternative to parsing dates from strings is to use the ```DateTime.strptime()``` method, which allows for more flexibility in specifying the format of the date string. However, this method requires more code and is not as beginner-friendly as the ```DateTime.parse()``` method.

## See Also:
- [DateTime.parse documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html#method-c-parse)
- [ISO 8601 standard](https://www.iso.org/iso-8601-date-and-time-format.html)
- [DateTime.strptime documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html#method-c-strptime)