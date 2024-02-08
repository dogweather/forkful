---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:40.447837-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
String interpolation lets you plug values into a string. It's handy to avoid messy concatenation and make code more readable.

## How to: (Як це зробити:)
```Ruby
name = "Іван"
greeting = "Привіт, #{name}!"
puts greeting # Виведе: Привіт, Іван!
```

Compound example with calculations:
```Ruby
hours_worked = 9
rate = 50
puts "Сьогодні ти заробив: #{hours_worked * rate} гривень." # Виведе: Сьогодні ти заробив: 450 гривень.
```

## Deep Dive (Поглиблений Розгляд)
Ruby first introduced string interpolation in version 1.8. It's cleaner than concatenation which uses '+':
```Ruby
# Without interpolation
puts 'Привіт, ' + name + '!'
```

You can't interpolate with single quotes, only double quotes or backticks:
```Ruby
# Won't work
puts 'Hello, #{name}!'
```

Interpolation automatically calls `to_s` on the variable:
```Ruby
# Even if name were a number, it'd convert it to a string:
name = 123
puts "Привіт, #{name}" # Виведе: Привіт, 123
```

Finally, when it comes to performance, interpolation is generally faster than concatenation.

## See Also (Дивіться також)
- The Ruby documentation provides details on string interpolation: https://ruby-doc.org/core-3.1.0/doc/syntax/literals_rdoc.html#label-Strings
- Practical use of string interpolation: https://www.rubyguides.com/2019/05/ruby-string-interpolation/
- For best practices when using string interpolation and concatenation: https://www.rubyguides.com/2020/01/ruby-string-concatenation/
