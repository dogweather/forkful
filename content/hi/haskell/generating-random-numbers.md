---
title:                "Haskell: संख्याओं का त्याग जेनरेट करना"
simple_title:         "संख्याओं का त्याग जेनरेट करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Kyon: 
Kisi ko prakrath algorithms ki madad se brahmaankan sankhyaon ka nirman kyu karna chahiye?

Koi bhi programming bhasha sikhne ke liye, dhyaan me rakhna jaruri hai ki yeh kis tarah se aur kyun kaam karti hai. Isi tarah, brahmaankan sankhyaon ka nirman bhi kisi dusre algorithms se alag hai aur iske pichhe kuch mukhya karan hain. Is blog post me hum dekhenge ki brahmaankan sankhyaon ka nirman kyu pratikool ho sakta hai aur kis tarah se ye hamari coding skills ko prabhavit karta hai.

##Kaise Kare:
```Haskell
main = do
  -- by using the "random" package
  import System.Random
  -- generates a random number between 1 and 10
  randomInt <- randomRIO (1, 10)
  print randomInt
```

Is code snippet me humne "random" package ka istemaal kiya hai jo hame kisi bhi range me ek random integer number deta hai. Isse hume ek nirdeshit range me kisi bhi tarah ka sankhya banana aasan ho jata hai. Hum bhi apne aapko ek seed value provide kar sakte hain taki hamesha same random number generate ho. Is tarah se, hamara output hamesha predictable hoga aur hame debugging karne me madad milegi.


##Gehri Jhalak:
Brahmaankan sankhyaon ka nirman ek bhavnatam prakriya hai programming me aur isse hamare coding skills ko develop hone me madad milti hai. Isse hume algorithms ko samajhne me madad milti hai aur iska istemaal bade bade project me jaise machine learning aur data analysis ke liye bhi kiya ja sakta hai. Iske alawa, random numbers ki khoj ke peeche bhi kaafi rachna hain aur ye ek bohot vyapak aur rochak vishay hai.


##Dekhna Jaroor:
Yadi aapko brahmaankan sankhyaon ka nirman ke bare me aur gehre jankariya chahiye, toh yahi samay hai ki aap "random" package ke documentation ko padhe aur iske ander ki programming techniques ko samajhe. Dhanyavaad!

See Also:
- [Haskell Programming Language](https://www.haskell.org/)
- [Random Package Documentation](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Introduction to Random Numbers in Programming](https://www.educative.io/edpresso/introduction-to-random-numbers-in-programming)