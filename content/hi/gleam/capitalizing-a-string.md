---
title:                "Gleam: स्ट्रिंग को मूल्यवर्धित करना"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

"## Kyun"
Kisi bhi programming language mein, string ko capitalize karna bahut common hai. Lekin Gleam mein isko karne ka ek unique aur aasaan tarika hai. Is blog post mein hum jaanenge ki capitalizing string kyu important hai aur iske kya benefits hai.

"## Kaise Kare"
Capitalizing string Gleam mein karna bahut hi simple hai. Sabse pehle, humein string ko define karna hoga. Fir hum `String.capitalize/1` function ka use karenge, jiska kaam hai string ko capitalize karna. Hum isko `|>` operator ke through bhi execute kar sakte hai.

"```Gleam
let name = "john"
let capitalized_name = name |> String.capitalize/1
IO.println(capitalized_name)
```
Output: "John"

Humne yaha pe ek variable `name` define kiya hai jisko humne `"john"` assign kiya hai. Fir humne `|>` operator se `String.capitalize/1` function ko `name` variable pe execute kiya hai aur uski output `capitalized_name` variable mein store kiya hai. Fir bas humne `IO.println` function ka use karke `capitalized_name` variable ki output print kar di.

Ab jab bhi hum `capitalized_name` variable ko print karwayenge, uski output capitalized form mein aayegi.

"## Gehri Jhaank"
String ko capitalize karna kafi common hai, lekin kya aapko pata hai ki Gleam mein iske liye kya process hota hai? Gleam mein string ko manipulate karne ke liye Unicode code point ka use kiya jata hai. Isse hum characters ko manipulate kar sakte hai aur unke case ko bhi change kar sakte hai.

Iske alawa, Gleam mein `String.capitalize/1` function ke alawa aur bhi functions hai jaise `String.uppercase/1` aur `String.lowercase/1` jo ki string ke case ko change karne ke liye use kiye ja sakte hai.

"## Dekhiye Bhi"
Agar aap aur jyada Gleam programming ke bare mein jaanna chahte hai, toh yeh links aapke liye helpful honge:
- [Official Gleam documentation](https://gleam.run/documentation/)
- [Gleam tutorial series](https://dev.to/codingthly/learn-gleam-programming-language-part-1-introduction-4lno)

Happy coding!