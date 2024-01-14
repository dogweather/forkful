---
title:    "Gleam: दो तारीखों को तुलना करना"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Kyon

Kisi bhi programming language mein, tarikhon ka tulna karna ek aam prashna hai. Yeh aksar kisi project mein aavashyak hota hai, jahan humein do tarikhon ke beech antar ko jaanna hota hai. Iske alava, hum kai baar tarikhein compare karke kisi condition ko check karte hain, jaise ki koi tarikh aaj se pahle ya baad mein hai ya nahi. Isliye, tarikhon ka tulna karna bahut zaruri hai aur isse programming skills ko improve karne mein bhi madad milti hai.

# Kaise

Tarikhon ka tulna karna Gleam programming language mein bahut hi aasan hai. Hum "Date" module ka upyog kar sakte hain, jismein "compare" function hain jo do tarikhon ko compare karta hai aur uska result humein "Order" enum ke roop mein deta hai. Iske alava, hum "Calendar" module ka bhi upyog kar sakte hain jismein "Days" function hain jo do tarikhon ke beech ke dinon ka antar calculate karta hai. Neeche diye gaye code blocks mein iska example diya gaya hai.

```Gleam
import Date
import Calendar

// Do tarikhon ka tulna

Date1 = Date.parse("2021-01-01")
Date2 = Date.parse("2021-03-05")

if Date.compare(Date1, Date2) == Order.Less {
  "Date1 iss Date2 se pahle hai"
} else if Date.compare(Date1, Date2) == Order.Greater {
  "Date1 iss Date2 se baad hai"
} else {
  "Dono tarikhon mein koi antar nahi hai"
}

// Tarikhon ke beech ke dinon ka antar

diff = Calendar.days(Date1, Date2)

"Date1 aur Date2 ke beech mein", diff, "din ka antar hai"
```

# Gahrai mein samaadhan

Tarikhon ka tulna karna asan lag sakta hai, lekin ismein gahrai hoti hai jo experienced programmers ko bhi challenges provide karti hai. Isse aap apne programming skills ko develop kar sakte hain aur effective solutions banane mein madad milti hai. Isliye, is prashna mein gahrai mein samaadhan khojna bhi zaruri hai.

# Dekhein bhi

Is article mein humne tarikhon ka tulna karna seekha. Agar aapko Gleam programming language aur humari articles pasand aayi hain, toh aap neeche diye gaye links mein se kuch aur bhi articles aur resources dekh sakte hain.

- [Official Gleam Documentation](https://gleam.run/documentation)
- [Gleam GitHub Repository](https://github.com/gleam-lang/gleam)
- [Introduction to Functional Programming in Gleam](https://medium.com/@tuzz/functional-programming-in-gleam-dba9943a5b2c)