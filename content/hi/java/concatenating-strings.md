---
title:                "Java: स्ट्रिंग जोड़ना"
simple_title:         "स्ट्रिंग जोड़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun
Ab aap soch rahe honge ki kya zaroorat hai string concatenation ki? Suniye, string concatenation ek important concept hai Java programming mein. Isse aap apne code mein alag-alag strings ko ek saath jod sakte hain aur usme se ek naya string bana sakte hain. Aaiye ab jaane ki kaise.

## Kaise Karein
String concatenation karne ke liye aapko 2 methods ka use karna hoga - "+" ya "concat()". Neeche diye gaye example mein aap dekh sakte hain ki kaise aap in methods ka use karke strings ko ek saath jod sakte hain.

```Java
// Use of "+" method
String str1 = "Hello";
String str2 = "World";
String str3 = str1 + " " + str2;
System.out.println(str3); // Output: Hello World

// Use of "concat()" method
String str1 = "She";
String str2 = "is";
String str3 = "beautiful";
String str4 = str1.concat(" ").concat(str2).concat(" ").concat(str3);
System.out.println(str4); // Output: She is beautiful
```

## Gehraai Mein Jaya
String concatenation ke alawa bhi kuch important points hain jinhe aapko jaanna zaroori hai. Sabse pehle, agar aap digits ko string mein add karna chahte hain, toh aapko unhe pehle string mein convert karna hoga. Uske baad aap inhe concatenate kar sakte hain. Dusra important point hai ki string concatenation ek efficient process nahi hai, isliye agar aap bahut saare strings ko concatenate karte hain, toh performance mein thoda impact pad sakta hai.

## Dekhiye Bhi
Agar aapko Java programming ki aur bhi concepts jaanne hain, toh aap in links ko dekh sakte hain:

- ["Java Strings" by W3Schools](https://www.w3schools.com/java/java_strings.asp)
- ["String Concatenation in Java" by GeeksforGeeks](https://www.geeksforgeeks.org/string-concatenation-in-java/)
- ["All About Strings in Java" by Oracle](https://docs.oracle.com/javase/tutorial/java/data/strings.html)

Happy coding!