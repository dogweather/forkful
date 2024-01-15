---
title:                "टेस्ट लिखना"
html_title:           "Gleam: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Kyun
Kisi bhi programming language mein, test likhna ek zaroori step hota hai jisse hamare code ki quality aur functionality ko sahi tarah se ensure kiya ja sake. Gleam mein bhi, test likhna ek important aspect hai jisse aap apne code ko safe aur reliable bana sakte hai.

## Kaise Kare
Agar aapne abhi tak test likhna nahi sikha hai, toh Gleam aapke liye ek great language hai jisme test likhna asaan aur effective hai. Neeche diye gaye examples ke saath dekhein:

```Gleam
test "Add function should return the sum" {
  assert.add(2,2) == 4
}
```

Is code block mein, humne ek test likha hai jisse hamne apne "add" function ke output ko check kiya hai. Agar add function mein koi galat code hota, toh test fail ho jata hai aur hume pata chalta hai ki kuch issue hai aur hume code ko fix karna hai.

## Deep Dive
Test likhne ke liye, sabse pehle aapko "test" keyword ka use karna hota hai. Iske baad aap test ka naam likhte hai aur brackets ke andar apne code ko likhte hai jisse aap test karna chahte hai. Iske baad hum "assert" keyword ka use karte hai jisse hum check karte hai ki kya humara code sahi output produce kar raha hai ya nahi. Agar assert ki value true hoti hai, toh test pass ho jata hai, aur agar false hoti hai toh test fail ho jata hai.

## Dekhiye
"See Also"  
- [Gleam Documentation](https://gleam.run/documentation/)
- [Effective Testing in Gleam blog post](https://medium.com/alwaysacloud/effective-testing-in-gleam-f9b8acd4c3c5)
- [Gleam Tests YouTube tutorial](https://www.youtube.com/watch?v=87NA-wMwK4E)