---
title:                "Haskell: स्ट्रिंग की लंबाई ढूंढना"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Kyun

Kya aapne kabhi socha hai ki kaise hum ek string ki lambai ko pata kar sakte hain? Shayad aapke dimaag mein yeh sawal aaya ho, kyunki iska koi practical use toh hai nahi, lekin yeh ek important programming concept hai jo aapko samajhna chahiye. Is blog post mein hum aapko batayenge ki kyun aur kaise hum ek string ki lambai ko find kar sakte hain Haskell mein.

## Kaise

Haskell mein, hum ek string ki lambai ko `length` function ka use karke pata kar sakte hain. Is function ko use karne ke liye, hume pehle ek string declare karna hoga aur fir `length` function ka use karke uski lambai find karni hogi. Neeche di gayi coding example mein aap dekh sakte hain ki kaise hum is function ko use kar sakte hain:

```Haskell
import Data.List

main = do
 let string = "Hello World!" -- string declare karna
 print $ length string -- lambai find karna
```

Sample Output:

```
12
```

Jaise ki aap dekh sakte hain, humne pehle string `Hello World!` ko declare kiya aur fir uski lambai `length` function ka use karke pata ki. Is tarah se hum kisi bhi string ki lambai ko find kar sakte hain.

## Deep Dive

Ab jab humne `length` function ka use karke string ki lambai find kar li, toh kya humein iss function ke baare mein kuch aur jaanna jaruri hai? Ek baat jo humein samajhni chahiye ki yeh function humein `Data.List` module mein milta hai, toh agar hum iss module ko import nahi karte hain, toh hum `length` function ka use nahi kar sakte. 

Iss function ko use karne ke liye, hume ek list ya array pass karna hota hai, jiski length hum find karna chahte hain. Iske baad, yeh function hume `Int` datatype ka output return karta hai, jiske through hum string ki lambai pata kar sakte hain. Yeh function bahut important hai, kyunki jab humein kisi bhi kaam mein strings ka use karna hota hai, toh humein unki lambai pata hona bahut zaroori hai.

# Dekhte Hain

Is blog post mein humne aapse share kiya ki kaise aap Haskell mein ek string ki lambai ko find kar sakte hain. Humne coding example aur sample output ke through aapko dikhaya ki kaise yeh kaam kiya ja sakta hai. Agar aapko aur bhi Haskell programming se related topics ke baare mein jaanna hai, toh neeche diye gaye links aapke kaam aa sakte hain:

- [Haskell for Beginners (Hindi)](https://www.indiaprogramming.com/haskell/haskell-for-beginners-hindi/)
- [Pattern Matching in Haskell (Hindi)](https://www.indiaprogramming.com/haskell/pattern-matching-haskell-hindi/)
- [Understanding Lambda Functions in Haskell (Hindi)](https://www.indiaprogramming.com/haskell/lambda-functions-haskell-hindi/)

# Dekhte Hain

Humein ummeed hai ki aapko yeh blog post pasand aaya hoga aur aapne kuch naya sikh liya hoga. Agar aapko isse related koi bhi doubts ya suggestions hain, toh humare neeche diye gaye social media handles pe humse connect kar sakte hain. Happy coding!