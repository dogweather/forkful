---
title:                "नई परियोजना शुरू करना"
html_title:           "Haskell: नई परियोजना शुरू करना"
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Kyon: Naye project shuru karne ka *kyon* karein?

Agar aap naye samay me kuch naya sikhna chahte hai aur tezi se grow karna chahte hai, toh naye project shuru karna ek accha vikalp ho sakta hai. Haskell se shuru karne me madad karega aur aapko functional programming aur strong typing concepts samjhne me help karega.

## Kaise Shuru Karein: Kood ke Udaaharan aur Sample Output

Haskell me code likhna kaafi aasaan hai. Yaha kuch udaharan hai jo aapko help karenge naye project shuru karne me:

```
-- Hello World
main = putStrLn "Namaste, duniya!"

-- Simple math operation
add :: Int -> Int -> Int
add x y = x + y

-- Output: 15
add 5 10
```

Yaha `main` function ko dekhein jo program ka entry point hai. `putStrLn` function string ko terminal me print karne ke liye use hota hai. Dusra udaharan me `add` function hai jo do integers ka sum return karta hai. Phir `add` function ko 5 aur 10 ke saath call kiya jata hai. Iska output 15 hoga.

## Gehri Jhanjhavat

Naya project shuru karne se pehle kuch points ka dhyan rakhna important hai. Haskell me code likhte waqt type safety ka dhyan dena jaruri hai. Type errors ko handle karna kaafi aasan hai aur code ki quality aur maintainability ko improve karta hai. Haskell me mutable variables nahi hote jisse concurrency issues kam hote hai.

Naya project shuru karne se pehle ek acchi design aur architecture plan karna bhi important hai. Functional programming ki philosophy me pure functions ka use kiya jata hai jo side-effects se door hote hai. Isse maintainability aur testing asaan hota hai.

## Jahan Dekhein

- [Haskell Tutorial](https://www.haskell.org/tutorial/)
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)

## Dekhne ke liye

- [Haskell Meets Github](https://haskellmeet.github.io/)
- [Haskell Discord Server](https://discordapp.com/invite/CGDWfqT)
- [Haskell Subreddit](https://www.reddit.com/r/haskell/)