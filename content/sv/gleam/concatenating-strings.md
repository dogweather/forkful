---
title:    "Gleam: Sammanslagning av strängar"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför du skulle vilja lära dig att sammanfoga strängar i Gleam. Svaret är enkelt - genom att använda denna funktion kan du enkelt kombinera flera strängar för att skapa en längre och mer komplex sträng. Detta är särskilt användbart vid formatering av text eller skapa dynamiska meddelanden.

## Hur man gör
För att kunna sammanfoga strängar i Gleam, behöver du använda operatorn "+" tillsammans med två eller flera strängar. Här är ett exempel på hur du kan använda det:

```Gleam
let förnamn = "Anna"
let efternamn = "Svensson"
let namn = förnamn + " " + efternamn
```

I detta exempel används "+" för att sammanfoga tre strängar - förnamn, efternamn och ett mellanslag. Resultatet blir en ny sträng som kombinerar båda namnen med ett mellanslag mellan dem. Output för detta exempel skulle vara "Anna Svensson". Du kan också sammanfoga flera strängar samtidigt genom att använda flera "+" operatorer, som i exemplet nedan:

```Gleam
let frukt = "äpplen"
let grönsaker = "morötter"
let andra_matvaror = "smör, mjölk"
let inköpslista = frukt + ", " + grönsaker + " och " + andra_matvaror
```

Output för detta exempel skulle vara "äpplen, morötter och smör, mjölk".

## Fördjupning
Överst på din inköpslista stod det "1 liter äpplejuice". Nu kanske du undrar hur du kan kombinera en sträng med en siffra för att skapa en komplett mening. Det finns ett antal sätt att göra detta på, men en enkel metod är att använda funktionen "to_string" för att konvertera talet till en sträng. Här är ett exempel på hur det kan se ut:

```Gleam
let juice_volym = 1
let juice_mängd = " liter"
let juice = juice_volym |> to_string |> +juice_mängd
```

Här används först funktionen "to_string" för att konvertera talet 1 till strängen "1", sedan sammankopplas den med strängen "liter" genom att använda "+".

"juice" skulle då vara "1 liter". Nu kan du enkelt sammanfoga olika datatyper för att skapa mer komplexa strängar.

## Se också
- "Gleam - En helt ny funktional programmeringsspråk": https://gleam.run/
- "Enhance Your Coding Skills with Gleam Programming Language": https://medium.com/@pierrenereya/enhance-your-coding-skills-with-gleam-programming-language-ebf697f98bcc