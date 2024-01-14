---
title:                "Haskell: सबस्ट्रिंग निकालना"
simple_title:         "सबस्ट्रिंग निकालना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Kyon

Substring extraction ek kaafi aasan aur uttam tarika hai jo aapke kaam ko saral aur sahi banata hai. Iska upyog bahut se programming tasks mein ho sakta hai jaise ki string manipulation, data cleaning aur aisi anya kaam.

## Kaise Karein

```Haskell
-- Substring ko start aur end index se extract karna
substring :: Int -> Int -> [a] -> [a]
substring start end list = take (end - start + 1) (drop start list)

-- Example
main = do
  let str = "Haskell is amazing"
  let result = substring 8 13 str
  print result
```

Is code ko run karne par aapko `is ama` ka output milega. Isi tarah aap apne code mein alag-alag starting aur ending index ka upyog karke alag alag substrings extract kar sakte hain.

## Gehri Jhaank

Substring extraction ke peeche kaaran hai ki humara samay aur mehnat bachta hai. Ek baar hume substrings ki prakriya samajh aa jaati hai, tab hum bahut se alag alag aur kaam ke tarike dhoondh sakte hain jisse hum apne code ko aur bhi efficient banate hain.

Iske saath hi, substring extraction ek bahut hi flexible process hai jisme hume shuru aur ant ke index ke alawa aur bhi bahut se chunne ke options hote hain jaise ki specific words, characters ya patterns ko bhi extract kar sakte hain.

## Dekhein Bhi

- [Haskell Strings](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html)
- [Substring Functions in Haskell](https://www.geeksforgeeks.org/haskell-substring-functions/)
- [A Beginner's Guide to Substrings in Haskell](https://www.codewars.com/kata/a-beginners-guide-to-substrings-in-haskell)