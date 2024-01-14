---
title:                "TypeScript: तारीख को स्ट्रिंग में बदलना"
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyon

Kisi bhi programming language mein, tithiyon ko ek string mein badalna zaroori ho sakta hai. Ye dates aur times ko ek readable format mein rakhne ke liye zaroori hai. TypeScript mein bhi, date ko string mein convert karna aasaan hai aur iss blog post mein hum aapko iss process ki sahi tarika batayenge.

## Kaise Karein

Date ko string mein convert karne ke liye, hum `toLocaleString()` method ka istemaal karenge. Iske liye, hum sabse pehle ek `Date` object banayenge. Fir `toLocaleString()` method ko iss object pe call karenge aur parameters ke roop mein hum desired date or time format ko specify karenge. Chaliye shuru karte hain!

```TypeScript
let currentDate = new Date(); 
console.log(currentDate.toLocaleString('en-US', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' } ));
```
Yeh humare console mein kuch iss tarah ki output dega: "Wednesday, 2021, June 23". Aap apne desired format ke hisaab se parameters change kar sakte hain.

Ab agar hum sirf time aur date ko alag alag parts mein print karna chahte hain, toh hum `toLocaleTimeString()` aur `toLocaleDateString()` methods ka istemaal kar sakte hain.

```TypeScript
console.log(currentDate.toLocaleTimeString('en-US'));
console.log(currentDate.toLocaleDateString('en-US'));
```

Ismein humare console mein time aur date alag alag tarah se print honge: "11:05:00 AM" aur "06/23/2021".

## Gehri Jhaank

Hamesha accha practise hoga ke hum `toLocaleString()` method ka `override` property bhi istemaal karein. Iss property se hum apne date aur time format ko customize kar sakte hain. `override` property hume ek `Date` object return karega, jiske saath hum `toLocaleString()` method ko fir se call kar sakte hain aur desired format ko specify kar sakte hain.

```TypeScript
let currentDate = new Date();
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
let dateToString = currentDate.toLocaleString('en-US', options);
console.log(dateToString); // Output: "Wednesday, 2021, June 23"

let customDate = dateToString.toLocaleString('en-US', { weekday: 'short', year: '2-digit', month: 'numeric', day: 'numeric' });
console.log(customDate); // Output: "Wed, 21, 6/23"
```

Iss tarah hum apne date aur time format ko apne requirements ke hisaab se customize kar sakte hain.

## See Also

Iss blog post mein humne dekha ki hum kaise TypeScript mein date ko string mein convert kar sakte hain. Agar aapko aur zyada jaankari chahiye inbuilt methods ke baare mein toh aap iss [documentation](https://www.typescriptlang.org/docs/handbook/internationalization.html) ko check kar sakte hain.

Agar aapko ye blog post helpful laga ho, toh humein [Twitter](https://twitter.com/techtouhid) pe zaroor follow karein. Aur agar aapko koi aur TypeScript ya programming se related topic pe blog post chahiye ho, toh humein [email](mailto:techblog@example.com) karke bata sakte hain. Happy coding! 🚀