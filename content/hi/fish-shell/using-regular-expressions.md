---
title:                "नियमित अभिव्यक्तियों का उपयोग"
html_title:           "Fish Shell: नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Kyun?

Kya aap code me cheezein search karne ke liye bheed bhaad ka shor-sharaba nahi karna chahte? Kya aapko specific patterns ko find aur manipulate karna hai? Yadi haan, to aapko regular expressions ka istemaal karna chahiye!

## Kaise?

Fish Shell me regular expressions ka use karna bahut hi aasaan hai. Aap simply "grep" ka istemaal karke kisi bhi file ya directory me specific patterns ko search kar sakte hai. Neeche diye gaye code block me ek example hai:

```Fish Shell
grep '[a-z] fish.txt'
```

Is code se, aap sabhi lines ko search kar sakte hai jo "fish.txt" file me lowercase letters se shuru hoti hai. Output me aapko yeh lines dikhayi denge.

```
big fish
small fish
```

## Deep Dive

Regular expressions me kayi tarike ke expressions ka istemaal karke aap apne search ko aur bhi precise bana sakte hai. Jaise ki aap specific characters, words, ya numbers ko search kar sakte hai. Iske alawa, aapko searching ke liye operators bhi available hai, jaise ki "or" ya "not". In operators ka istemaal karke aap apne patterns ko aur bhi specific bana sakte hai.

Fish Shell me, regular expressions ka use upyukt syntax ke saath kiya ja sakta hai, jisse ki aap exact output ko find aur manipulate kar sake. Iske alawa, Fish Shell me kayi useful functions bhi available hai, jaise ki "fish_grep" aur "fish_match", jo ki regular expressions ka use karke output ko filter aur manipulate karte hai.

## Dekhiye bhi

Ab aap regular expressions ke baare me basic jaankari rakhte hai, aap mazeed explore kar sakte hai yhaan dive into regular expressions ke saath regular expressions ke detailed tutorial ke saath.

1. [Regular Expressions tutorial by regexone.com](https://regexone.com)
2. [Learn to use regular expressions in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_regex)