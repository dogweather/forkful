---
title:                "दो तिथियों की तुलना करना"
html_title:           "Kotlin: दो तिथियों की तुलना करना"
simple_title:         "दो तिथियों की तुलना करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyun

Aksar hume apne project mein do tarikhon ko compare karna padta hai, jaise ki kisi event ke beech ki duration ya kisi task ke khatam hone ki date ka check karna. Kotlin mein tareekhon ka comparison bahut hi easy hai aur is article mein hum seekhenge ki kaise hum do tarikhon ko easily compare karke apne project ko aur efficient bana sakte hain.

## Kaise Karein

```Kotlin 
// Do tarikhon ko date format mein declare karein
val date1 = LocalDate.parse("2021-07-15")
val date2 = LocalDate.parse("2021-07-20")

// Date1 se date2 ko compare karein
if(date1 < date2) {
    println("Date1 pahle aata hai date2 se")
} else if (date1 == date2) {
    println("Date1 aur date2 ek saath hain")
} else {
    println("Date2 pahle aata hai date1 se")
}
```

### Output:

```
Date1 pahle aata hai date2 se
```

Is code snippet mein humne `LocalDate` class ka use kiya hai, jo ki Kotlin mein dates ko represent karne ke liye available hai. Iske baad humne `if` aur `else if` statements ka use kiya hai, jahan humne dates ko compare kiya hai aur unke beech difference check kiya hai. Is tarah se hum do tarikhon ko aasani se compare kar sakte hain.

## Deep Dive

Iss code snippet mein humne `LocalDate` class ka use kiya, jo ki Java 8 se available hai aur `java.time` package mein hai. Is class mein hum date aur time ko represent kar sakte hain, jaise ki year, month, day, hour, minute, second, etc. Iske alawa `LocalDate` class mein aur bhi functions hai, jinse hum date operations jaise ki addition, subtraction, etc. kar sakte hain.

`LocalDate` class ka use karne ke liye sabse pehle humein uss class ka object create karna padta hai, jismein hum date ko `parse` function se represent karte hain. Iske baad hum `if` aur `else if` statements ka use karke dates ko compare karte hain. Is tarah se hum iss kaam ko bohot hi asaan bana sakte hain.

## See Also

- [Kotlin Documentation on Dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [GeeksForGeeks article on Comparing Dates in Kotlin](https://www.geeksforgeeks.org/date-comparison-within-a-range-in-kotlin/)