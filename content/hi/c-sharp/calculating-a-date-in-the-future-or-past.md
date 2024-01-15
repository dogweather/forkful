---
title:                "भविष्य या भूतकाल में तारीख की गणना"
html_title:           "C#: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyon

Kisi bhi programming language mein, tareekh ko future mein ya past mein calculate karna ek aam kaam hai. Isse hum apne project mein precise date ko use kar sakte hain aur apni application ko advanced aur dynamic bana sakte hain. C# programming language mein bhi tareekh ko calculate karna bahut asaan hai, aur is article mein hum aapko iski jaankari denge.

## Kaise

Tareekh ko future mein ya past mein calculate karne ke liye, sabse pehle hum DateTime class ka istemaal karenge. Is class mein, hum tareekh ko store aur manipulate kar sakte hain. Sabse pehle, hum DateTime object create karenge, jismein hum current date ko store karenge. Fir hum 'Add' method ka istemaal karke desired value ko add karenge, jaise ki saal, mahina, din ya ghante. Iske baad, humara final date ready ho jayega. Yehi date humein future ya past mein calculate karne ke liye kaam aayega. Neeche diye gaye code snippet mein yeh saari steps ko dikhaya gaya hai:

```C#
// Create a DateTime object and store the current date
DateTime today = DateTime.Today;

// Add 1 year to the current date
DateTime futureDate = today.AddYears(1);

// Print the final date
Console.WriteLine("Future date: ", futureDate);
```

**Output:**

```
Future date: 05/04/2022
```

Isi tarah, hum kisi bhi desired value ko add karke future ya past date calculate kar sakte hain. Iske alawa, DateTime class mein aur bhi kai useful methods hain, jinhe aap explore kar sakte hain.

## Deep Dive

DateTime class mein, hum tareekh ko manipulate karne ke liye kai useful properties aur methods pa sakte hain. Some of the most commonly used ones are:

1. **AddDays()**: Yeh method humein current date mein desired number of days add karne se set karta hai.
2. **AddMonths()**: Is method ki madad se hum kisi bhi tareekh mein desired number of months add kar sakte hain.
3. **AddYears()**: Yeh method humein current date mein desired number of years add karne se set karta hai.
4. **AddHours()**, **AddMinutes()**, **AddSeconds()**: In methods ko use karke hum tareekh mein desired time ko add kar sakte hain.
5. **DayOfWeek**: Is property ke istemaal se hum kisi bhi tareekh ka din pata kar sakte hain.

Iske alawa, DateTime class mein kai aur methods aur properties hain, jinhe aap internet par search karke aur explore karke apne kaam mein istemaal kar sakte hain.

## See Also

1. [DateTime Class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
2. [DateTime Methods in C#](https://www.dotnetperls.com/datetime)
3. [Calculate Past or Future Date in C#](https://www.c-sharpcorner.com/article/how-to-calculate-past-or-future-dates-in-c-sharp/)