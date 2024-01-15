---
title:                "टेक्स्ट को ढूंढ़ना और बदलना"
html_title:           "Elm: टेक्स्ट को ढूंढ़ना और बदलना"
simple_title:         "टेक्स्ट को ढूंढ़ना और बदलना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun

Kai baar hamko kisi bhi text ko replace karna hota hai, jaise ki kuch words delete ya modify karna ho ya phir koi specific pattern ya format ko find karna ho. Elm mein hai ek built-in function "String.replace" jo text ko replace karne mein kaafi madad karta hai.

## Kaise Kare

Ham is function ko "String.replace" ka naam se use kar sakte hai. Iske liye hame do strings aur ek pattern dena hoga jo hum replace karna chahte hai. Example ke liye, agar hum ek string ko do parts mein split karna chahte hai, jahan split point ' ' hai, toh hum "String.replace" ka use kar sakte hai:

```Elm
String.replace "Hello Elm!" " " ", "  -- Output: "Hello, Elm!"
```

Is code mein pehle string " " ka jagah "Hello, Elm!" string mein ' ' ke jagah ", " replace hogaya hai.

Agar hum kisi specific format ko find karna chahte hai, jaise ki email addresses, toh hum is function ke sath "Regex.fromString" bhi use kar sakte hai. Example ke liye, ek string mein se sabhi email addresses ko find karke unhe replace karna ho, toh hum ye code use kar sakte hai:

```Elm
String.replace (Regex.fromString "[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}") " " "********"  -- Output: "********, ********"
```

Is code mein humne ek regular expression pattern diya hai jisse hum sabhi email addresses ko find karke unhe "********" se replace kar rahe hai.

## Deep Dive

"String.replace" function adding feature rich hai, jismein hum "Regex" bhi use kar sakte hai. Ye ek powerful tool hai text operations ke liye, jisse hum text ko split, replace, find aur modify kar sakte hai. Iske alawa, hum is function mein multiple patterns bhi use kar sakte hai aur isse humare kaam ko aur bhi asaan banate hai.

Ek important baat jo dhyaan rakna hai is function ke use mein, wo ye ki agar pattern match nahi hota hai string mein toh wo string khud hi return hogi. Isliye hamesha ek check karle ki pattern sahi hai ya nahi, taki hum unexpected output se bach sake.

## See Also
 - Elm String Documentation: https://package.elm-lang.org/packages/elm/core/latest/String
 - Regex Documentation: https://package.elm-lang.org/packages/elm/regex/latest/
 - String Replace Package: https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-Extra#replace