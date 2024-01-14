---
title:                "Javascript: स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Kyun
Agar aap ek Hindi programmer ho toh aapko shayad string ki length ko nikalna ke bare mein jaankari honi chahiye. String ka length nikalna bahut zaroori hai kyunki isse aap apne code mein string ko sahi tarah se use kar sakte ho aur uske saath alag alag operations perform kar sakte ho.

# Kaise Karein
```Javascript
let string = "Namaste";
console.log(string.length); // Output: 7
```

Is code block mein humne ek string variable banaya hai aur uska length property use karke uski length nikali hai. Is tarah se aap kisi bhi string ka length aasani se nikal sakte ho.

```Javascript
let emptyString = "";
console.log(emptyString.length); // Output: 0
```

Agar aap ek empty string ka length nikalna chahte ho toh yeh bhi possible hai. Empty string ka length 0 hota hai.

# Gehri Jhaank
String ka length nikalne ke liye, Javascript mein hum `.length` property ka use karte hain. Asal mein yeh ek special property nahi hai, balki yeh `.length` ki jagah `.length()` function ke roop mein bhi likha jaa sakta hai. Lekin, Javascript mein yeh already ek built-in property ke roop mein available hai, isliye hum aksar isse property ke tarah hi likhte hain.

Iske alawa, Javascript mein `.length` property string ke saath hi nahi balki arrays aur objects ke saath bhi use kiya jaa sakta hai. Arrays aur objects ka length unme present elements ya properties ki sankhya ko bataata hai. Isse aap arrays aur objects ko sahi tarah se manipulate kar sakte ho.

# Dekhiye Bhi
Agar aapko Javascript ke aur bhi concepts aur functions ke bare mein jaanna hai toh aap in links ko check kar sakte hain:

- [String Methods in Javascript (Hindi)](https://developer.mozilla.org/hi/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Arrays in Javascript (Hindi)](https://www.geeksforgeeks.org/javascript-array/)
- [Objects in Javascript (Hindi)](https://www.studytonight.com/javascript/objects-in-javascript.php)