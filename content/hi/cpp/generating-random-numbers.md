---
title:                "यादृच्छिक संख्याओं का उत्पादन"
html_title:           "C++: यादृच्छिक संख्याओं का उत्पादन"
simple_title:         "यादृच्छिक संख्याओं का उत्पादन"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyu

Kya aapne kabhi socha hai ki hamare computers ya mobile phones mein chalne wale games ya apps mein humesa different numbers, characters ya objects kyu dekhte hai? Ye sabhi random numbers se generate hote hai. Random numbers hamare computers aur programming languages ke liye ek important component hote hai. Ye hume different test cases, encryption, simulation aur games ke liye useful hote hai. Is article mein hum random numbers ke bare mein baat karenge aur dekhenge ki hum ise kis tarah generate kar sakte hai.

## Kaise

```C++
#include <iostream> 
#include <cstdlib> 
using namespace std; 
  
int main() { 
    // Yaha 0 se 99 tak ke random numbers generate kiye jaenge 
    for (int i=0; i<10; i++) { 
        int num = rand() % 100; 
        cout << num << endl; 
    } 
    return 0; 
} 
```

Output:

```C++
45 
12 
78 
54 
21 
60 
88 
17 
6 
92
```

Jaisa ki aap code mein dekh sakte hai, humne `#include <cstdlib>` header file ko use kiya hai jisme `rand()` function define hai. `rand()` function hume random numbers generate karne ki capability provide karta hai. Iske saath hi humne modulo operator `%` ka use kiya hai taaki hume specific range ke numbers mil sake. Yaha humne 0 se 99 tak ke numbers generate karne ke liye `% 100` use kiya hai. Aap apni marzi ke according range change kar sakte hai.

Lekin kya aap jante hai ki humara random numbers kitna random hota hai? Nahi na? Isliye chaliye hum `rand()` function ke deep dive mein jante hai.

## Gehraai Mein

Sabse pehle ye important hai ki hum random numbers ko generate nhi kar sakte, hum sirf pseudo-random numbers generate kar sakte hai. Matlab ki hum random numbers ki sequence ko repeat kar sakte hai. `rand()` function ek seed value leta hai jis se wo ek sequence follow karta hai. Seed value kisi bhi tarah ki input ho sakti hai jaise current time, user input, system clock, etc. Isliye agar seed value same hogi toh hume same random numbers milenge. Isse hume different range ke random numbers ki zarurat padegi jaise simulation ya encryption mein. Seed value ko `srand()` function se set kiya jata hai.

Random numbers ko more random banane ke liye different algorithms, seeds aur input sources ka use kiya jata hai. Lekin ek baat ka dhyan rakhe, humesha seed value change kare taaki hume alag alag sequence mile aur humara output unpredictable ho. Seed value ko handle karna bahut important hai random numbers ke generation ke liye.

## Dekhe Bhi

- [C++ rand() function](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C++ srand() function](https://www.geeksforgeeks.org/srand-in-ccpp/)
- [Pseudo-random number generation](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Generating random numbers in different ranges](https://www.geeksforgeeks.org/generating-random-number-range-c/)