---
title:    "Elixir: Att hitta längden på en sträng."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden av en sträng kan verka som en enkel uppgift, men det finns många praktiska tillämpningar för denna funktion. Förutom att räkna antalet tecken i en sträng kan det också hjälpa till med validering av inmatning och manipulering av data i en applikation. I denna artikel kommer vi att utforska hur man enkelt kan hitta längden av en sträng med hjälp av Elixir.

## Så här gör du

För att hitta längden av en sträng i Elixir, använder vi funktionen `length()`. Detta är en inbyggd funktion som tar in en sträng som argument och returnerar längden på strängen.

```Elixir
sträng = "Hej världen!"

längd = length(sträng)
IO.puts "Längden på strängen är #{längd}"
```

Output:

```
Längden på strängen är 13
```

Som du kan se i exemplet ovan använde vi interpolering för att infoga längden i vår `IO.puts` uttryck. Vi kan också använda denna funktion för att hitta längden på en lista eller tupel.

```Elixir
lista = [1, 2, 3, 4, 5]

längd = length(lista)
IO.puts "Längden på listan är #{längd}"
```

Output:

```
Längden på listan är 5
```

## Djupdykning

Det finns några saker som är viktiga att notera när det gäller att hitta längden av en sträng i Elixir. Funktionen `length()` returnerar antalet Unicode-kodpunkter i strängen, inte det faktiska antalet tecken. Detta kan orsaka problem om du arbetar med strängar som innehåller specialtecken eller emoji, eftersom de kan ha flera Unicode-kodpunkter.

För att få det faktiska antalet tecken i en sträng, kan vi använda funktionen `String.length()`. Denna funktion tar hänsyn till specialtecken och returnerar det korrekta antalet tecken i strängen.

```Elixir
sträng = "Hej 😊"

längd = length(sträng)
IO.puts "Antalet tecken i strängen är #{längd}"

längd = String.length(sträng)
IO.puts "Det korrekta antalet tecken i strängen är #{längd}"
```

Output:

```
Antalet tecken i strängen är 5
Det korrekta antalet tecken i strängen är 3
```

## Se även

- [Elixir Docs - Lista av inbyggda funktioner](https://hexdocs.pm/elixir/functions.html)
- [Elixir School - Strängar](https://elixirschool.com/sv/lessons/basics/strings/)