---
title:                "स्ट्रिंग को बड़ा लिखना"
html_title:           "Haskell: स्ट्रिंग को बड़ा लिखना"
simple_title:         "स्ट्रिंग को बड़ा लिखना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyon

Imagine ki aapke paas ek string hai, jisme kuch words hain jinke pehle letter small aur baaki letters capital hain. Agar aap chahte hain ki saare words capital letters se shuru ho, to aapko string ko capitalize karna hoga. Is article mein hum jaanege ki kaise aisa kiya ja sakta hai Haskell mein.

## Kaise Karein

Capitalizing ek string ko karne ke liye, aapko `toUpper` function ka use karna hoga. Is function ko `Data.Char` module mein define kiya gaya hai. Neeche diye gaye code block mein ek example diya gaya hai jisme hum ek string ko capitalize kar rahe hain aur output bhi show ho raha hai:

```Haskell
import Data.Char

capitalize :: String -> String
capitalize str = (toUpper (head str)) : (tail str)

main = do
    putStrLn "Enter a string: "
    str <- getLine
    putStrLn $ capitalize str
```

Output:

```
Enter a string: hello world
Hello world
```

## Deep Dive

`toUpper` function ko use karne se pehle, hum `Data.Char` module ko import kar lete hain. Iske baad hum `capitalize` function ko define karte hain jisme hum string ko split karke capital letters se replace karte hain. Is function ke bina bhi hum directly `toUpper` function ka use karke string ko capitalize kar sakte hain. Lekin agar hum function ko define karte hain, to hum is function ko baar-baar use kar sakte hain.

## Dekhein Bhi

- [Haskel mein functions ka use kaise karein](https://www.haskell.org/tutorial/functions.html)
- [Functional programming ka concept](https://www.geeksforgeeks.org/functional-programming-paradigm/)
- [Haskell ki official documentation](https://www.haskell.org/documentation/)