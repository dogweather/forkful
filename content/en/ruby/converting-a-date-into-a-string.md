---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in Ruby is transforming a date object into a readable string format that humans can understand. Programmers do this to output or store dates in a consistent and readable format.

## How to:

In Ruby, you can convert a date into a string using the 'strftime' method. Here's a typical usage:

```Ruby
date = Time.now
formatted_date = date.strftime("%d-%m-%Y")
puts formatted_date
```
The 'strftime' method formats Time according to the directives in the given format string. The directives begin with a percent (%) character. In this case, `%d-%m-%Y` will output a date in the format of Day-Month-Year like "27-02-2023".

## Deep Dive

The 'strftime' method has been part of Ruby since its early versions. Its name is short for 'string format time', and it has its roots in the C programming language.

There are other alternatives to format a date in Ruby, such as using the 'to_s' method with an argument:

```Ruby
date = Time.now
formatted_date = date.to_s(:db)
puts formatted_date
```
This will return the date in a database format like "2023-02-27 19:56:03".

As for implementation details, the way Ruby transforms a date into a string goes through a series of steps that involves breaking the date object into individual components (like year, month, day, etc.) and then assembling them into the desired format.

## See Also

For more details on date formatting and other options for the 'strftime' directive, check Ruby's official documentation: [https://ruby-doc.org/core/Time.html#method-i-strftime](https://ruby-doc.org/core/Time.html#method-i-strftime)

To see more about 'to_s' method and its arguments, you might want to read [https://ruby-doc.org/core/Time.html#method-i-to_s](https://ruby-doc.org/core/Time.html#method-i-to_s)

For more in-depth knowledge about how Ruby handles dates and times, this article is a good read: [https://www.jstorimer.com/blogs/workingwithcode/7766119-mastering-times-and-dates-in-ruby](https://www.jstorimer.com/blogs/workingwithcode/7766119-mastering-times-and-dates-in-ruby)