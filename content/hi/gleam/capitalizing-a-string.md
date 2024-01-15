---
title:                "स्ट्रिंग को बड़ा लिखें"
html_title:           "Gleam: स्ट्रिंग को बड़ा लिखें"
simple_title:         "स्ट्रिंग को बड़ा लिखें"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kya karne ka hai aapko kamiyaab
Agar aap kisi string ko capitalization karna chahte hai, jaise ki "hello world" ko "Hello World" mein badalna, to aapko ye article padhna chahiye! Iske liye, main aapko Gleam programming language ke bare mein batane ja rahi hu, jo ki ek functional programming language hai.

## Kaise karein
```Gleam
str.capitalize("hello world")
```
Yeh ek chota sa code hai jo aapko "Hello World" as an output dega. Isme humne `str.capitalize()` function ka use kiya hai jo string ko capital letters mein convert karta hai.

Ab, agar hum isme se kuch characters ko capitalize nahi karna chahte hai, to hum `str.uncapitalize()` function ka use kar sakte hai. Iske liye hume bas string ke sath function ka use karna hoga, jaise:
```Gleam
str.uncapitalize("Hello World")
```
Is code se hume "hello World" output milega.

## Gehri jhalak
Agar aapko aur gehri jankari chahiye string capitalize karne ke bare mein, to aap ye jaan sakte hai ki ye capitalization bas first letter ko uppercase karta hai aur baaki letters ko lowercase mein rakhta hai. Iske alawa, aap `str.uppercase()` aur `str.lowercase()` function ka bhi use kar sakte hai jo ki puri string ko uppercase aur lowercase mein convert karte hai.

## See Also
Iske alawa aur bhi useful Gleam resources hai jo aapke liye helpful ho sakte hai:
- [Gleam official website](https://gleam.run/)
- [Gleam Github repository](https://github.com/gleam-lang/gleam)

Umeed hai aapko ye article pasand aaya hai aur aapko kuch naya seekhne ko mila hai. Happy coding!