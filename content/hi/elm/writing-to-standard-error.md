---
title:                "स्टैंडर्ड त्रुटि में लिखना"
html_title:           "Elm: स्टैंडर्ड त्रुटि में लिखना"
simple_title:         "स्टैंडर्ड त्रुटि में लिखना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyun: Kaarn dwaara koi vyakti standard error kaa anuvaad karne me samay nikaalega, kaarn ki dushro gorvrn ki qurbaani karke hum apne code ko behtar tareeke se debug kar skte hain.

## Kaise Kare: Is prograaamig mee hmae adiaa  se standard errr ko kese likhaa?

```Elm
import Debug

Debug.crash "Ye ek sample error hai"
```

Output: `

Oops! Kuch galat ho gaya hai.
- Vakya: Ye ek sample error hai
- Lain: Null

## Gehraai Tak Jaayein: Dusra chizo mein samae bachakar lie, standard error ke baare mein adhik jaankari aapko bahut kaam aasakti hai. Isse, aap apne code ko behtar dhang se samajh sakte hain.

## Dekhein Bhi: Unn logon ke liye jinke saath aap apne Elm programming skills ko sudhaarna chate hain, humne kuch aur bhi articles likhe hain!

1. "Getting Started with Elm": [https://medium.com/@examplearticle1](https://medium.com/@examplearticle1)
2. "Debugging Techniques in Elm": [https://medium.com/@examplearticle2](https://medium.com/@examplearticle2)
3. "Elm Language Reference": [https://guide.elm-lang.org/](https://guide.elm-lang.org/)