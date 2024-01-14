---
title:    "Ruby recipe: Getting the current date"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
As a Ruby programmer, one of the most common tasks you may encounter is getting the current date. This could be for various reasons such as displaying the current date on your website or application, or for time-sensitive operations.

## How To
If you're using Ruby on Rails, the current date is readily available through the `Time.now` method. This will output the current date and time in the format: `YYYY-MM-DD HH:MM:SS +/-HHMM`.

```Ruby
current_date = Time.now
puts current_date
```

Output:
```
2020-06-24 11:30:00 +0530
```

If you prefer a specific date format, you can use the `strftime` method to format the output to your liking. Here are some examples:

```Ruby
current_date = Time.now
puts date.strftime("%d-%m-%Y") # outputs DD-MM-YYYY
puts date.strftime("%B %d, %Y") # outputs Month Day, Year
```

Output:
```
24-06-2020
June 24, 2020
```

## Deep Dive
Behind the scenes, Ruby uses the `Time` class to handle dates and times. This class has various methods such as `now`, `today`, and `zone` that can be used to manipulate and retrieve specific information about the current date and time.

It's important to note that `Time.now` uses the current system time, which can be affected by different time zones and daylight saving time. To avoid any inconsistencies, it's recommended to use the `Time.zone.now` method, which applies the appropriate time zone based on your Rails application settings.

Another useful method for retrieving the current date is `Date.today` from the `Date` class. This method only returns the current date without any time information.

## See Also
- [Time class documentation](https://ruby-doc.org/core-2.7.1/Time.html)
- [Date class documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)