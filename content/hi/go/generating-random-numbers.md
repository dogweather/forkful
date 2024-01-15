---
title:                "अनियमित अंक उत्पन्न करना"
html_title:           "Go: अनियमित अंक उत्पन्न करना"
simple_title:         "अनियमित अंक उत्पन्न करना"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## KyonKya aapne kabhi socha hai ki hamesha hamare paas apne computer ya mobile par different numbers ki zaroorat hoti hai? Iska matlab har bar manually numbers ko type karna padta hai. Lekin Go programming language ke saath, aap apne program mein random numbers generate kar sakte hain jisse aapka kaam aasan aur efficient ho jayega.

## Kaise Kare
Go language mein, random numbers generate karne ke liye `math/rand` package ka use kiya jata hai. Is package ko import karne ke baad, aap `rand.Intn()` function ka use karke numbers generate kar sakte hain. Ye function `n` tak random integers generate karta hai. Neeche diye gaye code block mein iska ek simple example hai:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  randomNum := rand.Intn(10)
  fmt.Println(randomNum)
}
```
Is code mein `math/rand` package import kiya gaya hai aur `rand.Intn()` function ka use karke `randomNum` variable mein ek random number store kiya gaya hai. Ye number 0 se 10 tak ka ho sakta hai. Sample output ke liye aap neeche diye gaye terminal output ko dekh sakte hain:

```
7
```

Agar aap chahte hain ki highest value bhi randomly generate ho, toh aap `rand.Int()` function ka use kar sakte hain. Is function mein aapko `min` aur `max` values specify karni hoti hain. Neeche diye gaye code mein iska ek example hai:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  randomNum := rand.Intn(10, 20)
  fmt.Println(randomNum)
}
```

Is code mein `randomNum` variable mein 10 se 20 tak ka random number store kiya gaya hai. Output ke liye aap neeche diye gaye terminal output ko dekh sakte hain:

```
18
```

## Deep Dive
Go language mein random numbers generate karne ke liye, internal pseudo-random number generator ka use kiya jata hai. Ye generator seed value ke basis par numbers generate karta hai. Seed value ko aap apne aap bhi set kar sakte hain, lekin agar aap ye nahi karte hain toh default seed value system clock ka use karti hai. Isse har baar program run hone par different numbers generate hote hain.

Go language mein `rand` package ke alawa bhi `crypto/rand` package available hai jismo cryptographic random numbers generate karne ke liye functions available hote hain. Ye numbers pseudorandom numbers se jyada secure hote hain aur ek random source se generate hote hain jo seed value use nahi karta.

## Dekhein Bhi
- [Official Documentation for `math/rand` package](https://golang.org/pkg/math/rand/)
- [Official Documentation for `crypto/rand` package](https://golang.org/pkg/crypto/rand/)
- [A Beginner's Guide to Random Numbers in Go (in English)](https://www.callicoder.com/golang-random-number-generation/)