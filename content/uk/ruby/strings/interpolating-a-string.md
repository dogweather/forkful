---
date: 2024-01-20 17:51:40.447837-07:00
description: String interpolation lets you plug values into a string. It's handy to
  avoid messy concatenation and make code more readable.
lastmod: '2024-02-25T18:49:47.598854-07:00'
model: gpt-4-1106-preview
summary: String interpolation lets you plug values into a string. It's handy to avoid
  messy concatenation and make code more readable.
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
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
