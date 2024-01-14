---
title:    "Ruby: अनियमित संख्याओं का उत्पादन"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Kyun
Random numbers ka utpadan karna kyun zaroori hai? Random numbers, humare dainik jeevan mein kai tarah ke kamo mein upyog hota hai, jaise cryptography, simulation, games, aur bahut kuch. Hum is blog post mein, Ruby programming language ke madhyam se random numbers ko kaise banaya ja sakta hai iska prarupan karenge.

## Kaise Karein
Random numbers banane ke liye, Ruby programming mein `rand` method ka upyog kiya ja sakta hai. Yeh method [0,1) yaani 0 se lekar 1 ke beech mein ek random decimal number return karta hai. Iske alawa, hum `rand` method mein ek argument bhi pass kar sakte hain jisse hume desired range mein random numbers milenge. Iske liye hum `rand` method ko `min` aur `max` ke beech mein arguments ke saath use kar sakte hain.

```Ruby
# Random decimal number
puts rand

# Random number between 0 to 10
puts rand(0..10)

# Random number between 50 to 100
puts rand(50..100)
```
Output:

```
0.8375630533884556
5
85
```

Iske alawa, hum `srand` method ka upyog karke, same random numbers ko generate kar sakte hain. Is method ko upyog karne se, humare code ka output hamein har baar same random numbers deta hai.

```Ruby
# Set seed for srand
srand(1234)

# Random decimal number
puts rand

# Random number between 0 to 10
puts rand(0..10)

# Random number between 50 to 100
puts rand(50..100)
```
Output:

```
0.1915194503788923
4
89
```

## Deeper Info
Random numbers banane ke liye, computer mein ek algorithm ka upyog kiya jaata hai jiska naam hai "Pseudorandom Number Generator". Yeh algorithm, ek seed value ka upyog karta hai jo ke sabhi random numbers ki base mein hoti hai. Seed value ko badalne se, hume different random numbers mil sakte hain. Ruby programming mein, `rand` method seed value ko automatically generate karta hai.

Iske alawa, hum `Random` class ka bhi upyog kar sakte hain random numbers banane ke liye. Isse hume aur bhi options aur control milta hai random numbers ke upar. Is class ke ek instance banane ke baad, hum `rand` method ki jagah `Random.rand` bhi use kar sakte hain.

```Ruby
# Generate random number between 0 to 10 using Random class
puts Random.rand(0..10)
```

## Dekho Bhi
Aur bhi random numbers ke baare mein jaanne ke liye, neeche mention kiye gaye links dekhein:

- [Ruby `rand` Method Documentation](https://ruby-doc.org/core-2.7.1/Random.html)
- [How Pseudorandom Number Generators Work](https://www.geeksforgeeks.org/how-to-generate-large-random-numbers/)
- [Seed value explained](https://www.thesprucecrafts.com/truly-random-seeds-in-a-pseudorandom-number-generator-2821221)