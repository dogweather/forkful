---
title:                "भाषा प्रोग्रामिंग पर एक लेख का शीर्षक: यादृच्छिक संख्याओं का उत्पादन"
html_title:           "Java: भाषा प्रोग्रामिंग पर एक लेख का शीर्षक: यादृच्छिक संख्याओं का उत्पादन"
simple_title:         "भाषा प्रोग्रामिंग पर एक लेख का शीर्षक: यादृच्छिक संख्याओं का उत्पादन"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyun
Random numbers ka upyog kyun kiya jaata hai? Random numbers ki vyavastha aapke program mein prabhaav ko badhaane ke liye squash tatha cryptographically secure communications mein iska upyog kiya ja sakta hai.

## Kaise
```Java
// Random class ka import
import java.util.Random;

// Object banaye
Random random = new Random();

// Integer random number generate karein
int randomNumber = random.nextInt();

// Diye gaye range ke beech mein random number generate karein
int randomNumberInRange = random.nextInt(100 - 50) + 50;

// Double random number generate karein
double randomDouble = random.nextDouble();

// Boolean random value generate karein
boolean randomBoolean = random.nextBoolean();

// Sample output
System.out.println("Random number: " + randomNumber);
System.out.println("Random number in range: " + randomNumberInRange);
System.out.println("Random double: " + randomDouble);
System.out.println("Random boolean: " + randomBoolean);
```

## Deep Dive
Random numbers ko generate karne ke liye, Java mein `random` class ka upyog hota hai. Is class mein `nextInt()` method se integer random numbers aur `nextDouble()` method se double random numbers generate kar sakte hain. Iske alawa `nextBoolean()` method se boolean random values bhi generate ki ja sakti hain. Is method ka upyog seed value ki madad se bhi kiya ja sakta hai, jisse same sequence mein random numbers generate kiya ja sake.

## See Also
- [Oracle Java Random Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java Randomization Tutorial](https://www.baeldung.com/java-random)