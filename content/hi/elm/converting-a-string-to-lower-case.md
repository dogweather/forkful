---
title:                "स्ट्रिंग को निचे केस में बदलना"
html_title:           "Elm: स्ट्रिंग को निचे केस में बदलना"
simple_title:         "स्ट्रिंग को निचे केस में बदलना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi socha hai ki aapke paas ek bada sa string hai aur aapko usse lower case mein convert karna hai? Shayad aap ek programming beginner ho aur apne string manipulation skills ko improve karna chahte ho ya phir aapko kisi project mein yeh functionality integrate karna hai. Iss article mein hum sikhege ki kaise hum Elm programming language mein strings ko lower case mein convert kar sakte hai.

## Kaise Kare
Agar aapko ek string ko lower case mein convert karna hai toh aapko `String.toLower` function ka use karna padega. Yeh function ek string value accept karta hai aur use lower case mein convert karke return karta hai.

```Elm
String.toLower "HELLO" -- Output: "hello"
```

Iske alawa, agar aap apne string variable ko lower case mein convert karna chahte hai toh aap `String.toLower` function ko direct string variable ke saath use kar sakte hai.

```Elm
stringVariable |> String.toLower

-- Old syntax
String.toLower(stringVariable)
```

Yeh ek aasan aur effective tareeka hai kisi bhi string ko lower case mein convert karne ka.

## Deep Dive
Ek string ko lower case mein convert karne ke liye, Elm internally Unicode standard ka use karta hai. Yeh ensure karta hai ki koi bhi language ya character set ka use karke string ko convert karna Unicode standard ke according ho. Aur yeh bhi guarantee karta hai ki aapka code internationalization-ready rehta hai.

Iske alawa, is function ka complexity O(n) hai, jaha n string length hai. Iss wajah se yeh bahut efficient tarika hai kisi bhi string ko lower case mein convert karne ka.

## Dekho Bhi
- [Official Elm Documentation for `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Elm Packages](https://package.elm-lang.org/)