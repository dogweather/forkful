---
title:                "उप-स्ट्रिंग निकालना"
html_title:           "Elixir: उप-स्ट्रिंग निकालना"
simple_title:         "उप-स्ट्रिंग निकालना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Kyu

Kabhi kabhi hamara code ek bada string ho jata hai aur hame kuch specific information extract karni hoti hai jaise ki sirf pahle naam ya last naam. Iss samasya ka samadhan karne ke liye hum substrings ka istemal karte hai.

## Kaise Kare

```Elixir
string = "John Doe"
IO.puts(String.slice(string, 0..3))
IO.puts(String.slice(string, 5..8))

```

Output:

```
John
Doe
```
Is code ke madhyam se aap dekh sakte hai ki humne `String.slice` function ka istemal kiya hai aur usne hame substring return ki hai jise humne range ke saath specify kiya hai. Range ki starting point aur ending point ko humne index numbers ki madad se specify kiya hai. Isse hume string ka specific portion mil jata hai.

## Deep Dive

Substrings extract karne ke liye Elixir me kai functions available hai jaise ki `String.replace`, `String.contains?`, `String.split`, etc. In functions ko istemal karke aap apne string ko manipulate kar sakte hai aur required information extract kar sakte hai.

Ek or interesting feature Elixir me hai string interpolation. Isme aap apne string me code add kar sakte hai aur wo code automatically execute ho jata hai. Isse aap apne string ko dynamic bana sakte hai aur substring extraction ko bhi asaan kar sakte hai.

## Dekhein Bhi

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [String Module Functions](https://hexdocs.pm/elixir/String.html#functions)
- [String Interpolation in Elixir](https://elixirschool.com/en/lessons/basics/string-interpolation/)