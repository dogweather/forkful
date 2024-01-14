---
title:                "Kotlin: यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyun

Random numbers kyun banane ka ek aasan tarika hai apne code mein unpredictability aur variability add karne ka. Isse aap apne code ko interesting aur versatile bana sakte hain.

## Kaise Kare

Random numbers banane ke liye, Kotlin mein `Random` class ka istemaal kiya jaata hai. Iske liye aap ye steps follow kar sakte hain:

1. `Random()` constructor ke saath ek object create karein.
2. `nextInt()` function ka use karke ek random integer generate karein.
3. Agar aap chahte hain ki random numbers ek specific range ke andar hi generate ho, toh `nextInt(n)` function ka use karein jahaan `n` aapke desired range ka ek number hai.
4. Agar aap chahte hain ki random numbers decimal values mein generate ho, toh `nextDouble()` function ka use kar sakte hain.

For example, agar hum chahte hain ki 1 se 10 ke beech random integers generate ho, toh hum ye code likh sakte hain:

```Kotlin
val random = Random()
val num = random.nextInt(10) + 1
println(num)
```
Iske output ke examples hain: 4, 8, 2, 10, etc.

Agar hum chahte hain ki random decimal values generate ho, toh hum ye code likh sakte hain:

```Kotlin
val random = Random()
val num = random.nextDouble()
println(num)
```
Output ke examples hain: 0.247593550982, 0.7658193592, 0.0512987413, etc.

## Gehraai Mein Jhaarnaa

Random numbers generate karne ke liye, `Random` class ke alawa bhi kuch aur methods hain jinhein aap istemaal kar sakte hain. Kuch examples hain:

1. `nextFloat()` function aapko random floating point numbers generate karne mein help karega.
2. `nextBoolean()` function aapko true ya false values random tarike se generate karne mein help karega.
3. Hum `Seed` concept se bhi eksath random numbers generate kar sakte hain. Seed basically ek starting point hota hai jisein computer seed kiya jaata hai aur woh uss point se random numbers generate karta hai. Iske liye aap `setSeed()` function ka use kar sakte hain aur seed ko specify kar sakte hain.

## Dekhiye Bhi

### See Also

- [Kotlin Random class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-random/index.html)
- [Random Number Generation in Kotlin](https://www.baeldung.com/kotlin/random)
- [Using Random Numbers in Kotlin](https://medium.com/@abbasalom/using-random-numbers-in-kotlin-b645d0d535cb)

Isse aapko random numbers generate karne mein koi problem nahi hogi. Ab aap apne code ko aur versatile bana sakte hain!