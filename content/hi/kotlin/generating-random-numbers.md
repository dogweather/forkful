---
title:    "Kotlin: कम्प्यूटर प्रोग्रामिंग में यादृच्छिक संख्याओं का उत्पादन"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Kyu

Kisi ke hirdye me aksar ek sawaal uthhta hai, ki "hum random numbers kyu generate karte hai?" Ek simple jawab hai ki random numbers hume apne code me unpredictability add karne ke liye madad karte hai. Yeh humari programs ko impreseed banata hai aur hume alag alag results deta hai har baar jab hum code ko chalate hai.

## Kaise Kare

Random numbers generate karne ke liye Kotlin me hume ek built-in function "random()" ka use karna hota hai. Is function ko use karke hume ek random number milta hai jo ki double data type me hota hai. Hum is function ko use kar sakte hai integers, floating point numbers ya kisi bhi number system ki values generate karne ke liye.

```Kotlin
// Randomly generate a number between 1 to 10
val randomNum = (1..10).random()
println("Random number: $randomNum")

// Output:
// Random number: 7
```

Hum agar chahte hai ki har baar jab hum code ko run kare toh hume ek alag random number mile toh hum "seed" property ka use kar sakte hai. Seed property hume ek seed value generate karta hai jise hum phir se use kar sakte hai code ko chalane ke baad bhi.

```Kotlin
// Generate a random number using a seed
val seed = (1..100).random()
val randomNum = Random(seed).nextInt(10) + 1 // Generates a number between 1 to 10
println("Random number: $randomNum")

// Output:
// Random number: 5
```

## Gehri Khudai

Random numbers generate karne ke liye hume samajhna hoga ki seed property kaise kaam karta hai. Seed property hume ek starting point deta hai jise hum use karke code me randomness lekar aate hai. Seed property ka use hume seed values dene me madad karta hai jaise current time, user input, ya koi bhi fixed value.

Seed property ka use hume code me predictability se bachata hai. Agar hum ek baar ek seed value use karte hai toh hum hai predict kar sakte hai ki next time bhi wohi value generate hogi. Lekin har baar agar hum alag alag seed values use kare toh hume unpredictable results milenge.

# Dekhe Bhi

Aap chahte hai ki apke random number generation skills ko aur behtar banaye? Niche diye gaye links par click karke aur seekhe!

- [Kotlin's built-in functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [Random number generation techniques in Kotlin](https://www.baeldung.com/kotlin/random)
- [Using the seed property in Kotlin](https://kotlinexpertise.com/kotlin-random/)