---
title:                "स्ट्रिंग को छोटे अक्षर में बदलना"
html_title:           "Fish Shell: स्ट्रिंग को छोटे अक्षर में बदलना"
simple_title:         "स्ट्रिंग को छोटे अक्षर में बदलना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun:
Aapne shayad kabhi na kabhi ek string ko lower case mein convert karne ki zarurat mehsoos ki hogi. Iska karan ho sakta hai ki aapko data ko sahi tareeke se sort karna hai ya fir aapki application mein case sensitivity ka issue hai. Whatever the reason may be, aapka solution hoga ek string ko lower case mein convert karna.

## Kaise Karein:
```Fish Shell``` mein aap ```string tolower``` command ka use karke ek string ko lower case mein convert kar sakte hain. Iske liye aapko yeh steps follow karne honge:
1. Sabse pehle, apna Fish Shell terminal open karein.
2. Ab ```string``` likhkar tab press karein, isse aapko sabhi string manipulation commands ka list milega.
3. Yahan se aap "tolower" command select karke enter press karein.
4. Ab aapko apna string enter karna hai jise aap lower case mein convert karna chahte hain.
5. Enter press karne ke baad, aap dekhenge ki aapka string lower case mein convert ho gaya hai.

```Fish Shell``` mein ```tolower``` command ka syntax yeh hai: ```string tolower <string>```

Iske alawa, aap ```string lower <string>``` ka bhi use kar sakte hain jo same result dega. Aap dono commands ko try karke dekh sakte hain.

## Deep Dive:
Ek string ko lower case mein convert karne ke liye, ```tolower``` command internally ```tr``` (translate) command ka use karta hai. Yeh command character by character text translation karta hai. Iske liye ```tolower``` command ko har ek character ko lower case mein convert karne ke liye diya jaata hai.

Agar aap interested hain, to aap ```man tolower``` command use karke iske aur details jaan sakte hain.

## See Also:
Yadi aapko ```Fish Shell``` ke aur commands aur features ke baare mein jaanna hai, to aap in links ko check kar sakte hain:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell Functions](https://fishshell.com/docs/3.1/commands.html#functions)