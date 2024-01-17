---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना।"
html_title:           "C#: तारीख को स्ट्रिंग में रूपांतरण करना।"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना।"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Wat & Kyu?
Jab hum kisi tareekh ko ek string mein badal dete hain, to hum usko stringification kehte hain. Programmers isko karne ke liye islie karte hain taki ve date ko alag alag tarike se display kar sakein ya fir use storage ya communication ke liye ready bana sakein. 

##Kaisay Karain?
```C#
// Sabse pehle, hum ek DateTime variable banaen
DateTime date = new DateTime(2021, 10, 31);

// Phir, hum is date ko string mein convert karenge
string dateString = date.ToString();

// Aap chahein toh apni marzi se date ka format bhi specify kar sakte hain
string customFormat = date.ToString("dd/MM/yyyy");

// Isko hum output mein dekh sakte hain
Console.WriteLine(dateString);
Console.WriteLine(customFormat);

// Output:
// 10/31/2021 12:00:00 AM
// 31/10/2021
```

##Gahraai Mein Jana
Date ko string mein badalne ka concept shuru se hi bahut important raha hai. Pehle, dates ko sirf numerical format mein store kiya jata tha, jaise 31/10/2021. Par jab computers aur programming languages mein Internationalization (I18N) ki concept aayi, tab date ka format aur display tarike bhi badal gaye. Ab programmers apni marzi se date ka format specify kar sakte hain aur various cultures aur languages mein alag alag format mein display kar sakte hain.

Ek aur alternative hai ki hum har date ke components ko alag alag variable mein store karke display kar sakein. Par ye zyada complexity aur space requirements ka kaaran banta hai.

Ek interesting baat ye hai ki stringification sirf dates ke liye hi nahi hota hai, hum kisi bhi data type ko ek string mein convert kar sakte hain, jaise numbers, booleans, ya objects. Isse hum apne data ko easily manipulate aur communicate kar sakte hain.

##Aur Parhain
Agar aapko C# programming aur stringification ke bare mein aur detailed information chahiye, toh ye links aapke kaam aa sakte hain:
- [Microsoft Docs: String Format for DateTime](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [GeeksforGeeks: C# | String Concatenation and Overloading of string](https://www.geeksforgeeks.org/c-sharp-string-concatenation-and-overloading-of-string/)

Happy stringifying! 🚀