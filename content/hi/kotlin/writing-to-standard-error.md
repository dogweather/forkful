---
title:    "Kotlin: स्टैंडर्ड त्रुटि पर लिखना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyun 
Kya aapne kabhi socha hai ki aapka code compile hone ke baad kisi bhi error ka saamna karna padega? Agar haan, toh aap standard error ki baat kar rahe hain. Standard error ka use hum apne code ke debugging mein karte hain. Isse aapka code ka flow samajhne mein aasani hoti hai aur aap errors ko sahi tareeke se handle kar sakte hain.

## Kaise Kare
Agar aapko apne Kotlin code mein standard error print karna hai, toh aapko `System.err.println()` ka use karna hoga. Ismein aap koi bhi object pass kar sakte hain jo aapko print karna hai, jaise ki ek string ya phir koi variable. Code snippet neeche diya gaya hai:

```Kotlin
val name = "Rahul"
System.err.println("Name: " + name)
```
Output: `Name: Rahul`

Is tarah se aap apne code mein standard error print kar sakte hain. Ab aap soch rahe honge ki yeh toh `println()` ke jaisa hi hai, toh fark kya hai? Fark yeh hai ki standard error output red color mein aata hai aur `println()` ka output white color mein aata hai, jisse aapko pata chal jaata hai ki kaunsa output standard error hai aur kaunsa normal output hai.

## Gehri Jaankari
Ab hum thodi gehri jaankari ke baare mein baat karenge. Agar aap standard error ko catch karna chahte hain, toh aap `try-catch` block ka use kar sakte hain. Jaise ki is code snippet mein dikhaya gaya hai:

```Kotlin
try {
    val number = "Hello".toInt()
} catch (e: NumberFormatException) {
    System.err.println("Invalid number format")
}
```
Ismein `Hello` string ko `toInt()` function mein pass karne se error generate ho raha hai, lekin humne ise `try-catch` block mein wrap karke standard error ko catch kar liya hai. Is tarah se aap apne code ke errors ko handle kar sakte hain.

## Dekhein Bhi
Agar aapko aur gehri jaankari chahiye standard error ke baare mein, toh aap niche diye gaye links par jarur dekhein:

- [Kotlin Documentation on Standard Streams](https://kotlinlang.org/docs/stdlib/jvm/internal/kotlin.io/-print-stream/index.html)
- [Java Exception Class](https://docs.oracle.com/javase/7/docs/api/java/lang/Exception.html)
- [How to Use try-catch Blocks in Kotlin](https://kotlinlang.org/docs/reference/exceptions.html)