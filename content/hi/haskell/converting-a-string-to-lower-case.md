---
title:                "Haskell: स्ट्रिंग को निचे अक्षर में बदलें"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun:

Kabhi kabhi ham yeh zaroorat mehsoos karte hain ki kisi string ko lower case mein convert karna hai, jaise ki agar hamare paas ek user se input liya gaya hai aur usko sahi tarah se validate karna hai. Is tarah ke situations mein, string ko lower case mein convert karna bohot zaroori ho sakta hai.

## Kaise Karein:

Is tarkib se string ko lower case mein convert karna bohot hi aasan hai:
```
Haskell
-- Samayogion ke liye, hamko Data.Char module import karna padega
import Data.Char

-- String ko lower case mein convert karein
toLower "HELLO WORLD"

-- Output: "hello world"
```

Yahan, hamne `Data.Char` module ko import kiya aur `toLower` function ka use kiya string ko lower case mein convert karne ke liye. Yeh function har ek character ko lower case mein convert karega aur naye string mein return karega.

Aur kuch tarkibon se bhi string ko lower case mein convert kiya ja sakta hai jaise ki `map toLower` ya phir `foldl (\acc x -> acc ++ [toLower x]) ""`.

## Gehri Jhaank:

String ko lower case mein convert karne ki gehrai mein, yeh jaanke kaam aata hai ki Haskell kaise characters ke saath kaam karta hai. Har character ko ek ASCII code se represent kiya jaata hai, jis se input aur output ko manipulate kiya ja sakta hai. `toLower` function ASCII code ke saath kaam karta hai, is liye yeh sirf alpha characters ko lower case mein convert karega aur kisi aur character ko change nahi karega.

## Dekhiye Bhi:

- [Haskell Data.Char module](http://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Learn You a Haskell - Strings](http://learnyouahaskell.com/starting-out#strings)