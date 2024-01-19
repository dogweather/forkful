---
title:                "Calculating a date in the future or past"
html_title:           "Ruby recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a future or past date, involves manipulating DateTime objects to derive a date relative to a specific point in time. Programmers do this to schedule tasks, analyze trends, or estimate deadlines.

## How to:
Ruby's built-in 'date' library makes this task a breeze. Let's dive into some examples:

```Ruby
require 'date'

# Current Date
current_date = Date.today
puts current_date
```

Run this to get today's date as output. Now, to calculate a future or past date:

```Ruby
# Future Date
future_date = current_date + 30
puts future_date

# Past Date
past_date = current_date - 15
puts past_date
```

The '+' and '-' operations add or subtract days from the given date. So, the output prints dates 30 days in future and 15 days in the past, respectively.

## Deep Dive

Historically, date manipulation was a bit challenging due to differences in calendar systems across cultures and the complexities of leap years. But modern programming languages like Ruby abstract these complexities through built-in libraries like 'date'. 

There's more than one way to manipulate dates in Ruby. You could use other classes like `Time` or ActiveSupport's `DateTime`. However, the `Date` class is often a simpler choice for purely date-focused tasks.

Here's a brief idea about implementation details - `Date.today + 30` works by creating a new `Date` object whose day is 30 days ahead of the current day. This is possible because the `Date` class in Ruby overloads the '+' and '-' operators.

## See Also
For more about the 'date' library, check out Ruby Date class docs here 
`https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html` 

For more advanced date and time manipulation check the ActiveSupport Time Extensions doc 
`https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html`