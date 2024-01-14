---
title:                "Ruby recipe: Getting the current date"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
As a beginner programmer, you may wonder why it's important to learn how to get the current date using Ruby. But in reality, it's a crucial skill that can make your code more efficient and dynamic. Knowing the current date can help you perform actions based on time, such as scheduling tasks or displaying time-sensitive information.

## How To
To get the current date in Ruby, we can use the `DateTime` class and its `now` method. Let's take a look at a simple code example:

```Ruby
date = DateTime.now
puts date
```

This code will output the current date and time in the following format: `YEAR-MONTH-DAYTHOUR:MINUTE:SECOND`. For example, `2020-07-14T11:23:30`. If you prefer a different date format, you can use `strftime` method to format the date as desired. For example:

```Ruby
date = DateTime.now
puts date.strftime("%d/%m/%Y")
```

This code will output the current date in the `DD/MM/YYYY` format, like `14/07/2020`.

## Deep Dive
Now, let's take a deeper dive into how the `DateTime` class works. The `now` method actually calls upon another method called `::now`, which is part of the `DateTime` module. This method creates a new `DateTime` object using the current date and time. The `DateTime` class also has various other useful methods such as `prev_day`, `next_day`, `prev_month`, `next_month`, etc. which allow you to easily manipulate dates.

Furthermore, the `DateTime` class is a subclass of the `Date` class, which is used specifically for handling dates without the time component. So, if you only need the date and not the time, you can use the `Date` class instead.

## See Also
If you want to explore more about the `DateTime` and `Date` classes, here are some useful resources:

- [Ruby DateTime class documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html)
- [Ruby Date class documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Ruby strftime documentation](https://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime)

Now that you know how to get the current date in Ruby, you can confidently incorporate it into your code and make it more dynamic and efficient. Happy coding!