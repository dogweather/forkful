---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
html_title:           "Elm: डायरेक्टरी मौजूद है या नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Ab hum baat karenge ki kisi bhi directory ka upasthit hona jaanchne mein kaun si bhumika hai. Directory existence jaanne ka kaam humare kaam mein kaafi important hai, kyun ki yeh hume bataata hai ki kya hum kisi specific path par kisi file ko read ya write kar sakte hai ya nahi.

## Kaise Karein

Agar aap directory ka upasthit hona jaanchne mein interested hai, to aap Elm ka use kar sakte hai. Directory jaanne ke liye, aap "Dir.exists" function ka use kar sakte hai. Is function ko fir se implement karna ki zaroorat nahi hai, kyun ki yeh already Elm core library ka part hai.

```Elm
Dir.exists "path/to/directory"
```
Yeh function "True" ya "False" return karega, jaha "True" directory ka existence indicate karta hai aur "False" uska absence. Agar aapko specific directory ka existence jaanna hai, to aap is function ko kisi variable mein store kar sakte hai, jaisa ki neeche diya gaya hai.

```Elm
exists = Dir.exists "path/to/directory"
```

## Gehra Jaankari

Yadi aapko aur gehri jaankari chahiye directory existence jaanne se judi, to aap elm-filesystem package ka use kar sakte hai. Is package ke andar FileSystem module hai jo kai aur functions provide karta hai. Ismein se ek function "listDir" hai jo ek list return karega, jismein humare directory ki files and subdirectories hongi.

See Also
- [Elm FileSystem package](https://package.elm-lang.org/packages/supermario/elm-filesystem/latest/)
- [Dir.exists function documentation](https://package.elm-lang.org/packages/elm/core/latest/Dir#exists)