---
title:                "Getting the current date"
html_title:           "Ruby recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Getting the current date is a common task in programming and can be useful for various purposes such as timestamping events or scheduling tasks.

## How To

To get the current date in Ruby, we can use the `Date` and `Time` classes. First, let's require these classes in our code:

```Ruby
require 'date'
require 'time'
```

### Using Date Class

We can use the `Date` class to get the current date in various formats. Let's look at a few examples:

```Ruby
# Get current date in YYYY-MM-DD format
Date.today.to_s # => 2021-06-07 

# Get current date in DD/MM/YYYY format
Date.today.strftime("%d/%m/%Y") # => 07/06/2021 

# Get current date in a custom format
Date.today.strftime("%B %d, %Y") # => June 07, 2021
```

### Using Time Class

The `Time` class can also be used to get the current date and time. Let's see an example:

```Ruby
# Get current date and time in ISO 8601 format
Time.now.iso8601 # => 2021-06-07T12:42:10+05:30
```

## Deep Dive

The `Date` and `Time` classes provide various methods for manipulating and formatting dates and times. To learn more about these classes and their methods, check out the official Ruby documentation for [Date](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/index.html) and [Time](https://ruby-doc.org/stdlib-3.0.1/libdoc/time/rdoc/index.html).

## See Also

- [Date Class Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/index.html)
- [Time Class Documentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/time/rdoc/index.html)