---
title:                "दो तारीखों का तुलना करना"
html_title:           "Elixir: दो तारीखों का तुलना करना"
simple_title:         "दो तारीखों का तुलना करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Aksar hume apne code mein do dates ko compare karna hota hai. Aise mein, Elixir mein date comparing ka concept bohot zaroori hai. Is article mein hum jaanenge ki dates ko compare kyun aur kaise karein.

## How To

Elixir mein dates ko compare karne ke liye aap DateTime module ka use kar sakte hain. Iske liye, sabse pehle aapko `DateTime` module ko import karna hoga:

```
defmodule DateComparison do
  import DateTime
end
```

Ab hum apne code mein do dates ko compare kar sakte hain. Iske liye, hum `DateTime.compare/2` function ka use karenge. Is function ke do arguments hote hain, jinmein se pehla date aur dusra date hota hai. Date format ka dhyaan rakhein, kyunki function sahi result tabhi dega jab dono dates ka format same ho.

```
DateTime.compare(%DateTime{year: 2020, month: 6, day: 5}, %DateTime{year: 2020, month: 5, day: 10})
=> 1
```

Is output se humein pata chalta hai ki pehli date badi hai dusri date se. Agar pehla date chota hota, to output -1 hota aur agar dono dates same hote, to output 0 hota.

Isi tarah, hum do dates ke beech mein difference bhi jaan sakte hain, `DateTime.diff/2` function ka use karke. Ye function milliseconds mein difference return karta hai.

```
DateTime.diff(%DateTime{year: 2020, month: 6, day: 5}, %DateTime{year: 2020, month: 5, day: 10})
=> 2592000000
```

## Deep Dive

Elixir mein, dates ko represent karne ke liye `DateTime` data type ka use kiya jata hai. Ye data type `year`, `month`, `day`, `hour`, `minute`, `second` aur `microsecond` keys ke sath hota hai. Isi tarah, saal aur din bhi calculate karne ke liye `Date` module ka use kiya jata hai.

Dates ko compare karte waqt, Elixir internaly unhe unix timestamps mein convert karta hai, jo ki milliseconds mein hota hai. Isi wajah se, results precise aur accurate hote hain.

## See Also

- Official Elixir documentation on DateTime module: https://hexdocs.pm/elixir/DateTime.html
- Official Elixir documentation on Date module: https://hexdocs.pm/elixir/Date.html