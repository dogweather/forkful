---
title:    "Elixir: Sammanslagning av textsträngar"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför
Elixir är ett kraftfullt programmeringsspråk som erbjuder många funktioner för att hantera data och processer. En av dessa funktioner är möjligheten att konkatinera strängar, vilket är användbart i många olika situationer.

## Hur man konkatinerar strängar
Konkatenering är helt enkelt processen att sammanfoga två eller flera strängar till en enda sträng. Detta kan göras på flera olika sätt i Elixir, men det enklaste sättet är att använda operatorn `<>`.

```Elixir
sträng1 = "Hej"
sträng2 = "världen!"
sträng1 <> sträng2
```
Output:
```Elixir
"Hej världen!"
```

Det är också möjligt att använda funktionen `String.concat/1` för att konkatinera flera strängar på en gång.

```Elixir
lista = ["Hej", "till", "alla"]
String.concat(lista)
```
Output:
```Elixir
"Hej till alla"
```

Elixir erbjuder också funktionen `String.Chars.to_string/1` för att konvertera andra datatyper till en sträng innan de konkatineras.

```Elixir
tal = 123
sträng = "Det här är ett tal: "
sträng <> String.Chars.to_string(tal)
```
Output:
```Elixir
"Det här är ett tal: 123"
```

## Fördjupning i konkatinering av strängar
När det kommer till konkatinering av strängar i Elixir finns det några saker att tänka på. Först och främst är det viktigt att hålla koll på vilken datatyp som används, eftersom vissa funktioner kräver att datatypen är en sträng.

Det är också möjligt att använda sig av placeholders istället för att skriva ut värdena direkt. Detta görs genom att använda `%`, följt av en siffra som representerar vilket index värdet har i listan.

```Elixir
tal1 = 10
tal2 = 5
"Summan av %1 och %2 är %3." <> String.Chars.to_string(tal1) <> String.Chars.to_string(tal2) <> String.Chars.to_string(tal1 + tal2)
```
Output:
```Elixir
"Summan av 10 och 5 är 15."
```

En annan viktig sak att notera är att Elixir erbjuder effektiva funktioner för att hantera stora mängder data, vilket är viktigt när man arbetar med konkatinering av strängar.

## Se även
- [Elixir's String module](https://hexdocs.pm/elixir/String.html)
- [Elixir's String.Chars module](https://hexdocs.pm/elixir/String.Chars.html)
- [Elixir's String.concat/1 function](https://hexdocs.pm/elixir/String.html#concatenation-and-interpolation)