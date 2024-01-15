---
title:                "नियमित अभिव्यक्तियों का उपयोग"
html_title:           "Java: नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

Why: Kyu Regular Expressions ka upyog karna hai?

Regular Expressions ek bahut prabhavi aur prachalit tool hai jo sahi pattern ko dhoondhne aur manipulate karne mein madad karta hai. Ye programming mein bahut kaam aata hai jaise data validation, data extraction, aur string manipulation. Isse kaam karte waqt hume bahut time aur effort bachata hai.

How To: Kaise Regular Expressions ka upyog karein?

Jaise ki humne pehle bhi kaha, Regular Expressions ka upyog karne ke liye hume ek pattern dhoondhna hota hai. Is pattern ko hum regular expression ya regex kehte hain. Iske baad hum uss regex ko Java mein implement karte hain.

```Java
// Regex Example
String regex = "a+[bc]{2}";
```

Iss code mein humne ek regex banaya hai jo "a" ke saath "bc" ya "bb" ko match karta hai. Ab hum ise kaise use kar sakte hain, ye dekhte hain.

```Java
String text = "abc";
boolean isMatch = text.matches(regex); // returns true
```

Is code mein humne `matches()` method ka upyog kiya hai jo humare text ko diye gaye regex ke saath match karta hai aur agar match ho jata hai to `true` return karta hai. Agar text me se "bc" hata kar "ab" banta hai to yeh match nahi karta aur `false` return karta hai.

Deep Dive: Regular Expressions ka upyog karte waqt hume kuch baato ka dhyan rakhna hota hai.

1. Escape Characters: Regex mein kuch characters special hote hain jaise `.` aur `*` jinki wajah se humare regex mein galti ho sakti hai. Isiliye hume en characters ke saath `\` ka use karna hota hai.

2. Character Classes: Regex mein hum `[ ]` ke andar characters ya ranges ka use kar sakte hain jaise `[a-z]`. Isse humare regex me kewal lowercase letters hi match honge.

3. Quantifiers: Ye Regular Expressions ke sabse powerful part hain. Inse hum specify kar sakte hain ki hum kisi character ya pattern ko kitni baar match karna chahte hain, jaise `a{3}` ye "aaa" ko match karega.

See Also: Dekhiye Regular Expressions ka upyog karte samay hume in cheezo ka dhyan rakhna hota hai. Regular Expressions ke aur bhi bahut saare features hain jo aap khud explore kar sakte hain.

Links: 
- [Tutorialspoint - Java Regular Expressions](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [GeeksforGeeks - Regular Expressions in Java](https://www.geeksforgeeks.org/regular-expressions-in-java/)
- [Oracle Docs - Java SE - Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)