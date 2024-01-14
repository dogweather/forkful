---
title:                "Elm: यादृच्छिक संख्याओं का उत्पादन"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyun:

Kya aapne kabhi socha hai ki kaise computer games mein har baar naye numbers generate hote hai? Ya kisi website par aapko random numbers ki zaroorat padti hai? Vaise toh humaare paas already numbers hote hai, phir kyun hume random numbers ki zaroorat padti hai? Well, iske peeche ka reason yeh hai ki har baar same numbers use karna boring ho jaata hai. Isse humare codes aur applications mein variation aata hai, jisse unhe interesting aur unpredictable banaaya ja sakta hai. Aur ek aisa tool hai jisse hum random numbers generate kar sakte hai - Elm programming language!

## Kaise Kare:

```Elm
import Random

randomNumber: Float
randomNumber =
    Random.float 0 100

main: Random.Float
main =
    randomNumber
```
Is code snippet mein humne Elm programming language ka use karke random numbers generate kiye hai. Sabse pehle humne `Random` library ko import kiya hai, phir apne `randomNumber` variable mein `Random.float` function ka use kiya hai jo 0 aur 100 ke beech ek random float number generate karta hai. Fir `main` function mein humne simply `randomNumber` ko return kiya hai. Agar aap yeh code run karenge, toh aapko har baar ek naya random number milega.

## Gehri Jhaank:

Random numbers generate karne ke peeche ka logic kuch iss tarah ka hai - humari systems mein ek random number generator hota hai jo ek seed value se start hota hai. Seed value basically ek starting point hota hai jisse har baar random numbers generate hote hai. Isse hume har baar naye numbers milte hai, aur seed value ko change karke hum different sets of numbers bhi generate kar sakte hai.

Random numbers generate karte waqt hume dhyaan rakhna hota hai ki hum seed value ko hafta ya ghante mein ek baar hi change karein, nahi toh humare numbers predict ho sakte hai. Isliye yeh practice hai ki hum seed value ko time based ya hardware based values se generate karte hai, jisse unpredictable aur truly random numbers generate hote hai.

## Dekhe Bhi:

- [Official Elm Random library documentation](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elm programming language guide](https://guide.elm-lang.org/)
- [Examples of using Elm for web development](https://github.com/mdgriffith/elm-style-animation)