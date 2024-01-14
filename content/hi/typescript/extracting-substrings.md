---
title:    "TypeScript: उपस्थित उपस्थितियों को निकलना"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Kyu

Substring extraction kyu karna chahiye? Substring extraction ek bahut hi common programming task hai. Jab hume ek string se specific characters ya words extract karne ki zaroorat hoti hai, tab hum substring extraction ka istemal karte hai. Isse hume zyada lines of code likhne ki zaroorat nahi padti aur hum apni code ko optimized bhi bana sakte hai.

# Kaise Karein

Substring extraction bahut hi saral hai aur TypeScript mein iska istemal bahut hi aasan hai. Hum `substring()` method ka istemal kar sakte hai jo hume ek string mein se specific characters ko extract karne mein madad karta hai. Iske liye hum `substring()` method ko string ke baad dot notation se call kar sakte hai.

```TypeScript
let str = "Hindi is a beautiful language!";
let substr = str.substring(0, 5);
console.log(substr); // Output: Hindi
```

Is code mein humne `str` variable ko `"Hindi is a beautiful language!"` string ke equal kiya hai. Fir humne `substring()` method ka istemal karke `substr` variable mein `str` string ke first 5 characters ko store kiya hai. Aur last mein humne `substr` ko console mein print karwaya.

# Dhyan Se Dekhein

Substring extraction mein kuch important points hai jo hume dhyan se dekhne ki zaroorat hai. 

- `substring()` method mein hume sirf starting index aur ending index dena hota hai. Yeh method starting index se lekar ending index ke pehle tak ke characters ko extract karta hai.
- Starting aur ending index dono inclusive hote hai, yaani ki humare jo ending index hai woh bhi extract hoga.
- Agar hume starting index se lekar end tak ke saare characters extract karne hai, toh hume sirf starting index dena hoga, ending index ki zaroorat nahi hogi.
- Hum `substring()` method mein negative values bhi de sakte hai. Agar hum negative value dete hai toh yeh method input ke end se lekar back count karta hai.

# Dekhein Bhi

- [MDN Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Codecademy Tutorial](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-string-functions/cheatsheet)