---
title:                "डीबग आउटपुट छापना"
html_title:           "Ruby: डीबग आउटपुट छापना"
simple_title:         "डीबग आउटपुट छापना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyun
Kisi ko debugging output pritnt karne mein kyun interest hai?

## Kaise Kare
```Ruby
# Example code to print debug output in Ruby
def square(x)
  puts "Calculating square of #{x}"
  result = x ** 2
  puts "Result: #{result}"
  return result
end

square(5)
```
Output:
```
Calculating square of 5
Result: 25
```

## Deep Dive
Debugging output ek useful tool hai jo humein code ke sahi chalne ki monitoring aur potential errors ko identify karne mein madad karta hai. Isse hum apne code ko better understand aur improve kar sakte hain. Iske alawa, kisi bhi user ka program ke functioning ko samajhne mein help milti hai.

Is tool ko use karne ke liye, hum `puts` ya `p` command ka use kar sakte hain jo humein apne code ki certain variables, functions ya errors ko detect karne mein help karta hai.

## See Also
* [Debugging in Ruby](https://www.honeybadger.io/blog/debugging-ruby/)
* [5 Tips for Effective Debugging in Ruby](https://medium.com/@annalauraschott/5-tips-for-effective-debugging-in-ruby-b8e47710a1a5)
* [How to Print Debug Output in Ruby](https://www.oreilly.com/library/view/ruby-cookbook-2nd/0596523696/ch05s03.html)