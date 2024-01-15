---
title:                "यादृच्छिक संख्याएं उत्पन्न करना"
html_title:           "Kotlin: यादृच्छिक संख्याएं उत्पन्न करना"
simple_title:         "यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyu: Kyu koi vyakti random sankhyao ka utpadan karega?
Random sankhyao ka utpadan bahut saari sthitiyon mein bahut upyogi hota hai, jaise ki koi bhi prakriya, game ya samikaran mein. Yeh sankhyaein bina kisi pratibhuti ke hoti hai aur har baar nayi hote hai, jisse har baar alag output prapt hota hai.

## Kaise Kare: 
```
Kotlin
// Random sankhya banane ke liye 
val random = Random()
val num = random.nextInt()

// Nirdharit range tak sankhya banane ke liye
val range = 1..10
val num = random.nextInt(range)
```

```
Kotlin
// Random floating point sankhya banane ke liye
val random = Random()
val num = random.nextDouble()

// Nirdharit range mein floating point sankhya banane ke liye
val range = 0.0..1.0
val num = random.nextDouble(range)
```

```
Kotlin
// Nirdharit range mein sankhya banane ke liye
val random = Random()
val num = random.nextInt(5, 10)
```

```
Kotlin
// Nirdharit sankhyao ko shuffel karne ke liye
val list = listOf(1, 2, 3, 4, 5)
val randomList = list.shuffled()
```


## Gehri Jankari: 
Random sankhyao ka utpadan karne ke liye, computer mein ek seed value ka upyog kiya jata hai. Ye seed value random number generator ke dwara randomly generate kiya jata hai. Seed value alag-alag samaanya activities mein upyog kiya jata hai jaise ki date, time, system ka geet, etc. Random number generator ke kuch mukhya algorithms hai jaise ki Lehmer Generator, Blum Blum Shubb Generator, etc. In algorithms mein randomness aur quality mein antar hota hai.

## Dekhein bhi:
Agar aapko random number generator ke bare mein aur gehri jankari leni hai, toh neeche diye gaye links jarur dekhein.
- [Random number generator ke bare mein gehri jankari (Hindi)](https://cs101bootcamp.github.io/posts/hindi/random-number-generation/)
- [Random number generator ke prakar (Hindi)](https://www.javatpoint.com/kotlin-random)
- [Randomness ka definition aur importance (Hindi)](https://www.geeksforgeeks.org/randomness-importance-generate-random-numbers-programming/)
- [Randomness ka scientific approach (Hindi)](https://cs.algosara