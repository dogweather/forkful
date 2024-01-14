---
title:    "Ruby recipe: Comparing two dates"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

As a software developer, you may encounter situations where you need to compare two dates in your Ruby programming. This could be important for tasks such as checking if an expiration date has passed or comparing timestamps for event scheduling. Knowing how to properly compare dates is a valuable skill to have in your programming arsenal.

## How To

To compare dates in Ruby, you can use the `==`, `<=`, `>=`, `<`, and `>` operators. These operators return a boolean value indicating whether the comparison is true or false. Let's take a look at some examples using the `Date` class in Ruby:

```
require 'date'

# Create two Date objects
date1 = Date.parse("2021-10-01")
date2 = Date.parse("2021-09-30")

# Compare dates using the `==` operator
puts date1 == date2  # Output: false

# Compare dates using the `<=` operator
puts date1 <= date2  # Output: false

# Compare dates using the `>=` operator
puts date1 >= date2  # Output: true 

# Compare dates using the `<` operator
puts date1 < date2  # Output: false 

# Compare dates using the `>` operator
puts date1 > date2  # Output: true
```

As you can see, by using these operators, we are able to compare two dates and get a boolean result indicating whether the comparison is true or false.

## Deep Dive

Behind the scenes, the `==` operator uses the `eql?` method to compare two dates. This method checks if the two dates have the same year, month, and day values. The other comparison operators use a combination of the `eql?` method and the `compare` method to determine the result. The `eql?` method is used to check if the two dates are equal, while the `compare` method is used to determine the relationship between the two dates.

Additionally, when comparing dates, it's important to consider time zones and daylight saving time. It's recommended to always use the `Time` class when dealing with time zones and daylight saving time in Ruby.

## See also

For further reading on comparing dates in Ruby, check out these resources:

- [Ruby's Date class documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby's Time class documentation](https://ruby-doc.org/core-2.7.2/Time.html)
- [How to deal with time zones and daylight saving time in Ruby](https://medium.com/@jacksoncage/dealing-with-time-in-ruby-part-2-time-zones-and-daylight-saving-time-7e6b1b967d30)