---
title:                "टेक्स्ट को खोजें और बदलें"
html_title:           "Javascript: टेक्स्ट को खोजें और बदलें"
simple_title:         "टेक्स्ट को खोजें और बदलें"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Kya aapne kabhi apne code mein ek specific text ko find aur replace kiya hai? Agar haan, toh aapko pata hoga ki yeh ek bahut common task hai jo har programmer ko karna padta hai. Text search and replace ka kaam karte waqt aapko same text ko manual search karne ki koi jarurat nahi hoti, jisse aapka time bachta hai aur aapka code bhi efficient banta hai.

## How To
```Javascript
// Yaha pe hum "Hello World!" ke saath ek variable declaration karenge.
var text = "Hello World!";
// Ab hum "Hello" ko "Hi" mein replace karenge.
var newText = text.replace("Hello", "Hi");
// Ab humara newText "Hi World!" hoga.
console.log(newText); // Output: "Hi World!"
```

Kya aapko dikh raha hai? Humne .replace() function ka use kiya jisse humne ek specific text ko find aur replace kiya. Is function mein pehle humne find karna chahe text ko aur fir uske replace karne wale text ko dena hota hai. Agar hume saare instances ko replace karna hai, toh hum .replace() function ke sath "g" modifier bhi laga sakte hai.

## Deep Dive
Jaise ki humne upar dekha, .replace() function ek string method hai jo ek string ko find aur replace karne ke liye use kiya jaata hai. Is function ke arguments mein pehla argument woh text hai jo hume replace karna hai, aur dusra argument woh text hai jisse hum replace karna chahte hai. Hum chahe toh .replace() function ka ek optional argument bhi use kar sakte hai jisme hum find karne ke liye ek regex pattern bhi specify kar sakte hai.

Ek important point yeh hai ki .replace() function, original string ko change nahi karta hai. Balki iska output ek completely new string hai jisme changes kiye gaye hai. Agar aap chahe toh original string ko bhi change kar sakte hai, iske liye hume ek variable mein yeh new string store karna hoga.

## See Also
Agar aapko Javascript mein .replace() function ke alawa bhi aur string methods ke bare mein janna hai, toh aap ye links check kar sakte hai:

- [MDN - String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#Instance_methods)
- [W3Schools - Javascript Strings](https://www.w3schools.com/js/js_strings.asp)

Main aasha karta hu ki aapko yeh article helpful raha hoga. Happy coding!