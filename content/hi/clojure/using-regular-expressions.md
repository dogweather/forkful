---
title:                "रेगुलर एक्सप्रेशंस का प्रयोग करना"
html_title:           "Clojure: रेगुलर एक्सप्रेशंस का प्रयोग करना"
simple_title:         "रेगुलर एक्सप्रेशंस का प्रयोग करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

Aapne kabhi socha hai ki hum kaise text ko check karte hai ki kya woh humare requirements se match karta hai ya nahi? Yeh kaam karne ke liye hum regular expressions ka istemal karte hai. Regular expressions ek tarah ka pattern hota hai jise hum text mein search kar sakte hai. Yeh text processing mein kaafi kaam aata hai aur isliye programmers ise istemal karte hai.

## What & Why?

Regular expressions ek tarah ka search pattern hota hai jo hum text mein apply kar sakte hai. Isse hum text ko extract kar, validate kar ya modify kar sakte hai. Regular expressions ko use karke hum bahut saare repetitive task ko automate kar sakte hai aur code ko concise aur efficient banane mein madad milta hai.

## How to:

```Clojure
;; Text se matched lines extract karna
(re-seq #"\d+\s[a-z]+" "1 apple, 2 bananas, 3 oranges")

;; Output:
("1 apple" "2 bananas" "3 oranges")

;; Text mein pattern search karna
(re-find #"clojure" "Clojure is awesome.")

;; Output:
"Clojure"

;; Text ko modify karna
(replace "awesome" "fun" "Clojure is awesome.")

;; Output:
"Clojure is fun."
```

## Deep Dive:

Regular expressions humare code mein flexibility laate hai, lekin iske use mein hume kuch factors ka bhi dhyan rakhna hota hai. Regular expressions mein syntax kaafi powerful hai aur hume iske sahi tarah se use karne ki practice karni chahiye. Iske alawa, iske kuch alternatives bhi hai jaise ki string functions aur parser libraries. Regular expressions ka use karne se performance mein kuch kami ho sakti hai, lekin is baat ka hume dhyan rakhna chahiye ki hum regular expressions ko limit mein hi istemal kare.

## See Also:

Regular expressions ke baare mein aur jaankari ke liye aap ye links dekh sakte hai:

- [Official Clojure documentation on regular expressions](https://clojure.org/api/cheatsheet)
- [Regular expressions tutorial by Derek Banas](https://youtu.be/rsO8o09Ull0)
- [Online regular expressions tester](https://regex101.com/)