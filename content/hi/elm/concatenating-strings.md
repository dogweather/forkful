---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Elm: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kya aur Kyun?
Ek samay tha jab strings ko jodne ke liye keval "+" operator ka upyog hota tha, lekin ab iska asli maqsad", string concatenation" ke roop me jaana jaata hai. Ismein ek ya adhik strings ko sath milkar ek hi string banaaya jaata hai. Programmers iska upyog generally texts, log messages aur APIs mein deta hai.

## Kaise karen?
Elm mein strings ko join karne ke liye "+" operator ka prayog kar sakte hain. Iske alawa, hum `append` function bhi use kar sakte hain.

```Elm
"Hello " + "World!"  -- output: "Hello World!"

append "Hello" "World!" -- output: "HelloWorld!"
```

## Gahare Jahaj
Is feature ki shuruwat C++ aur Java jaise languages mein hui aur aaj kal almost sabhi programming languages me aapko iska support mil jaayega. Alag alag languages mein thoda sa difference ho sakta hai ki kaunsa operator ya function upyog karna hai. Elm mein "+" operator ke alawa, hum `append` function bhi use kar sakte hain jo do strings ko join karke ek naya string bana deta hai.

## Aur Bhi Dekhein
Agar aapko strings ko manipulate karna ya join karna pasand hai toh aap [Elm documentation](https://guide.elm-lang.org/types/strings.html) se aur jankari prapt kar sakte hain. Iske alawa, aap `concat` aur `join` jaise functions ke bare mein bhi adhik jaan sakte hain.