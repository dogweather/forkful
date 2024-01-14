---
title:    "C#: तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Dates ko string mein convert karna ek bahut aam prashna hai jab hum C# programming mein kaam karte hain. Is post mein hum is prashna ka jawab dene wale hain aur aapko batayenge ki iska kya upyog hai.

## Kaise Karein
Dates ko string mein convert karne ke liye, hum `ToString()` method ka upyog karte hain. Is method ke zariye hum ek DateTime object ko specific format mein convert kar sakte hain. Neeche diye gaye code blocks mein aap dekh sakte hain ki kaise hum is method ka upyog kar sakte hain:

```C#
// DateTime object banayein
DateTime date = new DateTime(2021, 10, 15);

// DateTime object ko string mein convert karein
string strDate = date.ToString("dd/MM/yyyy");

// Convert ka output hai: 15/10/2021
Console.WriteLine("Convert ka output hai: " + strDate);
```

Ek aur tarika hai `ToString()` method ka upyog karne ka, jismein hum `Format()` function ka upyog karte hain. Is tareeke se hum specific format ke saath direct convert kar sakte hain. Neeche diye gaye code block mein aap isko bhi dekh sakte hain:

```C#
// DateTime object banayein
DateTime date = new DateTime(2021, 10, 15);

// DateTime object ko string mein convert karein
string strDate = string.Format("{0:MM-dd-yyyy}", date);

// Convert ka output hai: 10-15-2021
Console.WriteLine("Convert ka output hai: " + strDate);
```

## Gehraai Mein Jaaein
Agar aap ek developer hain, toh aapko pata hoga ki Dates ko string mein convert karne ke bahut sare tareeke hain. Humne upar sirf ek do tareeke dikhaye hain, lekin aap dekhenge toh C# mein aur bhi tareeke hain jiske zariye hum datetime object ko string mein convert kar sakte hain. Aap khud experiment karke aur alag-alag formats try karke dekh sakte hain ki kaise date ko hum string mein convert kar sakte hain.

## Dekhiye Bhi
Agar aap C# programming aur DateTime object ke bare mein aur jaankari chahte hain, toh neeche diye gaye links aapke kaam aayenge:

- [DateTime.ToString() method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Custom date and time format strings in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)