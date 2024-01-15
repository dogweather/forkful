---
title:                "एक तारीख को स्ट्रिंग में रूपांतरण करना।"
html_title:           "C#: एक तारीख को स्ट्रिंग में रूपांतरण करना।"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरण करना।"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyu
Ek common programming task hai ek tarikh ko ek string mein convert karna. Iske kai reasons ho sakte hain, jaise ki display purposes, database storage, ya information exchange.

## Kaise Kare
Tarikh ko string mein convert karne ke liye C# mein kuch simple steps hain:

```C#
// DateTime object create kare
DateTime date = new DateTime(2020, 12, 25);

// ToString() method use karke tarikh ko string mein convert kare
string dateString = date.ToString();

// Output: 12/25/2020 12:00:00 AM
Console.WriteLine(dateString);

// Agar specific format chahte hain toh ToString() method ko overload kare
string formattedDate = date.ToString("yyyy/MM/dd");

// Output: 2020/12/25
Console.WriteLine(formattedDate);
```

## Deep Dive
Tarikh ko string mein convert karne ke liye C# mein kuch important points hai. Pehle, ek DateTime object create karna zaruri hai. Ye object DateTime structure ka ek instance hota hai jo tarikh aur samay ko store karta hai. Phir ToString() method use karke hum apne DateTime object ko string mein convert kar sakte hain. Iski madad se hum different formats mein apni tarikh ko display kar sakte hain. Iske alawa, ToString() method ko overload karke hum specific format ke hisab se output bhi customize kar sakte hain.

See Also:
- https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-format-strings
- https://www.geeksforgeeks.org/c-sharp-datetime-structure-with-examples/