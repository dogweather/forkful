---
title:    "Gleam: स्ट्रिंग संयुक्त करना"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

"## Kyon"

Strings ko aapas mein jodne ka matlab hai ke hum alag-alag words ya sentences ko ek saath milakar ek bada string bana sake. Isse hamare program mein texts aur messages ko dynamic tarike se generate aur display karne mein madad milti hai. Is liye Gleam mein string concatenation bahut hi zaruri hai.

"## Kaise Kare"

Gleam mein strings ko jodne ke liye, hum "++" operator ka istemal karte hain. Isse hum do ya adhik strings ko aapas mein jod sakte hain. Neeche diye gaye code blocks mein samjhein:

```Gleam

// Ek example ke taur par, hum ek variable mein "Hello" string ko store karenge
let greeting = "Hello"

// Ab hum is variable ke saath "World" string ko jodenge
greeting ++ "World" 

// Output: HelloWorld
```

Aap dekh sakte hain ke "++" operator se "Hello" aur "World" strings ek saath jod kar "HelloWorld" bana di gayi hai. Is tarah se hum do ya zyada strings ko jod sakte hain.

"## Gehri Khurak"

Strings ko jodne ke alawa, Gleam mein hum "++" operator ke saath hi variables aur constants ko bhi jod sakte hain. Isse hume zyada flexibility milti hai apne program mein.

Iske alawa, Gleam mein bahut se functions aur methods bhi hain jo hume strings ko concatenate karne mein madad karte hain. In functions aur methods se hum string manipulation kar sakte hain, jaise ki strings ko reverse karna, uppercase ya lowercase karna, ya phir specific words ko replace karna.

"## Dekhein Bhi"

Agar aapko Gleam mein strings concatenate karna aur iska istemal karna aur seekhna hai, to aap in links par jaa kar is bare mein aur bhi gehri jaankari haasil kar sakte hain:

- https://gleam.run/examples/
- https://gleam.run/learn/documentation/
- https://gleam.run/learn/getting-started/

Is tarah se aap strings ko concatenate karne ke baare mein poori tarah se samajh sakte hain aur iska istemal kar sakte hain apne Gleam programs mein. Happy coding!