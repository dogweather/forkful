---
title:                "Att hitta längden på en sträng"
html_title:           "C#: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom programmering och är användbart i många olika sammanhang. Det kan till exempel vara för att kontrollera input från användare eller för att manipulera textsträngar på olika sätt.

## Så här gör du

```C#
string myString = "Det här är en sträng";

// Använd metoden Length för att hitta längden på strängen
int length = myString.Length;

Console.WriteLine(length); // output: 19
```

För att hitta längden på en sträng i C# kan vi använda metoden Length som finns tillgänglig för alla strängar. Metoden returnerar antalet tecken i strängen och är ett enkelt sätt att få fram längden på en sträng.

## Djupdykning

Det som faktiskt händer bakom kulisserna när vi använder metoden Length är att den tar emot en parameter av typen string och sedan räknar ut antalet tecken genom att iterera över varje enskilt tecken i strängen. Detta kan verka som en enkel uppgift, men det finns vissa saker att tänka på när vi arbetar med strängar och längden på dem.

För det första är det viktigt att komma ihåg att längden som returneras av metoden Length inte är samma sak som indexeringen av en sträng. Längden är antalet tecken i strängen medan indexeringen börjar på 0 och går upp till längden minus en. Detta kan orsaka problem om vi inte är medvetna om det, så det är alltid bra att dubbelkolla längden på en sträng innan vi räknar ut vilken karaktär som finns vid ett visst index.

Ett annat viktigt koncept att förstå är att längden på en sträng är dynamisk, vilket innebär att den kan ändras under programmets körningstid. Till exempel, om vi tar bort ett tecken från en sträng, kommer längden att minska. Detta kan påverka vårt program om vi gjort antaganden om längden när vi vill manipulera strängen på något sätt.

## Se även

- [Microsofts officiella dokumentation om metoden Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
- [En tutorial om strängar i C#](https://www.c-sharpcorner.com/UploadFile/ff2f08/strings-in-C-Sharp/)
- [C# for Beginners: Learn by Coding in C# (Udemy course)](https://www.udemy.com/course/c-sharp-tutorial-for-beginners/)