---
title:                "Using a debugger"
aliases:
- /en/ruby/using-a-debugger.md
date:                  2024-01-25T20:50:23.941247-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?

Using a debugger in Ruby gives programmers a superpower to pause their code, inspect variables, and step through their code line by line. Folks do it to squash bugs, understand code flow, and to see exactly what their written spells (code) are doing when the magic happens—or doesn't.

## How to:

Ruby comes with a built-in debugger called `byebug`. First, include `byebug` in your Gemfile and run `bundle install`. Then, plop `byebug` right where you want your program to take a breather.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Running this script will halt execution at `byebug`, and you'll be thrown into an interactive session where you can type commands like:

```
step
next
continue
var local
```

Sample output would give you a prompt looking like this:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Deep Dive:

Way back before `byebug`, Rubyists used `debugger` and `pry`. The latter, `pry`, is more than a debugger; it's a powerful REPL that can also be used for debugging with the `binding.pry` breakpoint.

Alternatives to Ruby's `byebug` include `pry-byebug`, which combines `pry` with `byebug` functionality, and `ruby-debug`, which is an older gem not actively maintained.

When you invoke `byebug`, the debugger suspends your code execution and gives you a peek into the runtime. You can see and change variables, jump to different points in the code, and even run some Ruby code line by line. It's kinda like having time-travel abilities for your Ruby code.

## See Also:

- Byebug GitHub Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry Documentation: [https://github.com/pry/pry](https://github.com/pry/pry)
- A Guide to Debugging Rails Apps: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
