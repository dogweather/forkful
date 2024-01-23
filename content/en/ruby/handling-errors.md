---
title:                "Handling errors"
date:                  2024-01-21T21:19:07.187007-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
---

{{< edit_this_page >}}

## What & Why?

Error handling is about expecting the unexpected in code — managing mistakes and problems gracefully without crashing. Programmers do it to control the flow when things go wrong and to keep the user experience smooth.

## How to:

Ruby uses `begin`, `rescue`, `ensure`, and `end` to handle errors. You wrap the risky code in `begin` and `end`. If an error occurs, `rescue` kicks in.

```Ruby
begin
  # Risky code goes here.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Oops! You can't do that: #{e.message}"
ensure
  puts "This always runs, error or not."
end
```

Sample Output:
```
Oops! You can't do that: divided by 0
This always runs, error or not.
```

## Deep Dive

Historically, error handling in programming languages has evolved significantly, with early languages often having crude or non-existent mechanisms. Ruby's exception handling is inspired by languages like Python and Smalltalk.

Alternatives to `begin-rescue` in Ruby include using `rescue` in method definitions or employing `throw` and `catch` for non-standard flow control, though they are not used for typical error handling.

One interesting detail: Ruby's exceptions are objects (instances of `Exception` class and its descendants), so you can define custom error classes and do more than just log errors — you can carry rich state around the program for more robust error handling.

## See Also

- The Ruby documentation on exceptions and error handling: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- A detailed guide on Ruby error handling best practices: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
