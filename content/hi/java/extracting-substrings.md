---
title:                "Java: उपस्थित सबस्ट्रिंग्स को निकालना (Upasthit substrings ko nikalna)"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Kyun:
Substring nikalne ka pehla aur sabse zaruri sawal hai ki hum aisa kyun karna chahte hain. Substring kya hota hai? Ye ek programming term hai jo string ke ek hisse ko alag se nikalta hai. Ye bahut useful ho sakta hai jab hume kisi specific information ko string se extract karna ho.

## Kaise Karein:
Substring extract karne ke liye, pehle humein original string ka pata hona chahiye. Fir hum iss string par kaam kar sakte hain aur usse substring banakar usse kisi variable mein store kar sakte hain. Iss process ko karne ke liye hum Java programming language ka use kar sakte hain. Neeche diye gaye code snippet mein hum ye dekhenge ki kaise hum substring extract kar sakte hain.

```Java
// Original string
String str = "Meri pehli blog post";

// Substring  "pehli" extract karne ke liye
String substr = str.substring(5, 10); 

// Output
System.out.println(substr); // "pehli"
```

Is code mein humne pehle original string "Meri pehli blog post" ko ek variable mein store kiya, fir uss string par kaam karke substring extract kiya aur usse ek alag variable mein store kiya. Fir humne output ko print kiya jisme humne dekha ki sirf "pehli" word extract hua hai. Is tarah hum kisi bhi string se substring extract kar sakte hain.

## Gehri Jankari:
Substring extract karne ke liye, hum string ke index numbers ka use karte hain. Index numbers hote hain string ke har character ka count karke assign kiya hua number. Ye process left se right hoti hai, matlab string ke pehle character ka index number 0 hoga, dusre ka 1, teesre ka 2 aur aise hi aage badhta jaega.

Substring extract karne ke liye, hum `substring()` method ka use karte hain. Is method mein hume do parameters pass karne hote hain - start index aur end index. Start index se substring shuru hota hai aur end index se string khatam ho jata hai. Yahan dyaan rakhe ki end index ke value mein hum last character ke index number se 1 subtract karte hain.

Agar hum sirf start index pass karte hain to end index string ke last character ka index number liya jaega. Agar hum sirf ek parameter pass karte hain to substring method uss index se lekar string ke last character tak ka part extract kar dega.

### See Also:
- Java Strings Tutorial (in Hindi): https://www.programiz.com/java-programming/string
- String substring() method docs: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-