---
title:                "भविष्य या अतीत में एक तारीख की गणना करना"
html_title:           "Kotlin: भविष्य या अतीत में एक तारीख की गणना करना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kya hai aur Kyun?
Calculating date in the future or past ek aam kaam hai jo programmers apni code mein karne ke liye karte hain. Isse aap ek date ke saath kisi specific interval ko add ya subtract kar sakte hain, jisse aap apni application mein dynamic date values use kar sakte hain.

## Kaise Karein:
Kotlin mein kisi date ke saath interval add ya subtract karne ke liye, aap ```java
LocalDate
``` class ka use kar sakte hain. Is class mein hume various methods milte hain jaise ki ```java minusDays(), plusMonths()``` jisse hum desired interval ke saath date calculate kar sakte hain. Neeche diye gaye code examples aur output aapko sahi samajhne mein madad karenge:

- Future date calculate karna:
```Kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(10)
println("Aaj ke baad 10 days ka sahi date hai: $futureDate")
```
Output:
```
Aaj ke baad 10 days ka sahi date hai: 2021-07-08
```
- Past date calculate karna:
```Kotlin
val today = LocalDate.now()
val pastDate = today.minusMonths(2)
println("Aaj se 2 months pehle ka sahi date hai: $pastDate")
```
Output:
```
Aaj se 2 months pehle ka sahi date hai: 2021-03-08
```

## Deeper Info:
Is date calculation ka concept bahut purana hai aur programming languages mein iska istemaal bahut dino se ho raha hai. Pehle log isko manually calculations se karte the, lekin ab isko coding ke through karna bahut hi aasan ho gaya hai. Iske alawa, aap iske alternatives jaise ki Java ke ```Date``` aur ```Calendar``` classes ka bhi use kar sakte hain, lekin Kotlin ki ```LocalDate``` class ismein bahut hi easy aur efficient hai.

## See Also:
Kotlin date calculation ke bare mein aur jaankari ke liye aap neeche diye gaye sources ko refer kar sakte hain:
- [Kotlin LocalDate class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Java Date and Time API](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)