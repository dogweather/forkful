---
title:                "Ruby: कंप्यूटर प्रोग्रामिंग पर लेख: रैंडम नंबरों का उत्पादन"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapko kabhi kabhi apne code mein kuch naya aur alag try karna pasand hai? Kya aapko khudse sawal puchhna aur uska jawab dhundhna pasand hai? Agar haan, to shayad random numbers generate karna aapke liye ek majedaar kaam ho sakta hai! Random numbers generate karne se, aap apne code mein surprise elements add kar sakte hai aur naye solutions explore kar sakte hai. Iss article mein hum dekhenge ki hum kaise Ruby mein random numbers generate kar sakte hai.

## Kaise Karein

Sabse pehle, hum randomNumber naam ka ek variable declare karenge, jisme hum random number store karenge. Uske baad, hum Random module ko use karke random number generate karenge. Neeche diye gaye code block mein aapko samajh aa jayega -

```Ruby
randomNumber = Random.rand(1..10)
puts "Aapka random number hai: #{randomNumber}"
```

Agar aap yeh code implement karenge, to har baar jab aapise run karenge, aapko ek naya random number milega. Code mein "1..10" humne yeh bataya hai ki aapko numbers ki range kya honi chahiye, aap apni marzi ke hisaab se iss range ko change kar sakte hai.

Ab, agar aap chahte hai ki aapko har baar same random number mile, to aap seed value ka use kar sakte hai. Seed value basically hume batata hai ki humare code mein kitna randomness chahiye. Seed value ko neeche diye gaye code block mein dekhiye -

```Ruby
randomNumber = Random.new_seed
puts "Aapka random number hai: #{randomNumber}"
```

Seed value har baar change hoti rahegi, isliye aapko har baar different random number milega.

## Gehri Jankari

Random number generate karne ke liye, Ruby mein kai saare methods available hai. Humne upar jo example diye hai, woh Random module ka ek method hai. Iske alava, aap "rand" method ka use karke bhi random numbers generate kar sakte hai. Isme aapko ek argument dena padega, jiski value aapko random number ki range batayegi.

Ruby mein, random numbers generate karne ke liye algorithm ka use hota hai. Woh algorithm "Mersenne Twister" hai, jo ki ek pseudorandom number generator hai. Pseudorandom number generator matlab woh number generate karta hai jo ki random hi lagta hai, lekin uske seed value se hi generate hota hai.

## Dekhiye Bhi

- [Ruby Random Module] (https://ruby-doc.org/core-2.6.3/Random.html)
- [Pseudorandom Number Generator] (https://www.rubyguides.com/2016/05/pseudo-random-numbers/)
- [Difference Between rand and Random] (https://www.geeksforgeeks.org/difference-between-rand-and-random-ruby/)