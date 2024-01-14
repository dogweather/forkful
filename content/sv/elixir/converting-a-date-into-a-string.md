---
title:    "Elixir: Omvandla ett datum till en sträng"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift inom Elixir-programmering, särskilt när man arbetar med externa API:er eller behöver presentera datumet på ett visst sätt för användaren. Det kan också hjälpa till att sortera eller filtrera data baserat på datum. I denna bloggpost kommer vi att utforska hur man konverterar ett datum till en sträng i Elixir.

## Så här gör du

För att konvertera ett datum till en sträng i Elixir, behöver vi använda funktionen `DateTime.to_iso8601/1` eller `DateTime.to_string/2`. Dessa funktioner tar ett `DateTime`-objekt som argument och returnerar en sträng i ISO 8601-format eller ett anpassat format, baserat på angivna alternativ.

Låt oss titta på ett exempel på hur man använder `DateTime.to_iso8601/1`:

```elixir
DateTime.to_iso8601(~U[2020-10-10 12:00:00])
```

Output:

```
"2020-10-10T12:00:00Z"
```

Vi kan också använda `DateTime.to_string/2` för att konvertera ett datum till en sträng i ett anpassat format. Det första argumentet är fortfarande ett `DateTime`-objekt och det andra argumentet är en sträng som definierar formatet. Vi kommer att använda "YYYY-MM-DD HH:MM:SS" för att få ut samma resultat som ovan.

```elixir
DateTime.to_string(~U[2020-10-10 12:00:00], "YYYY-MM-DD HH:MM:SS")
```

Output:

```
"2020-10-10 12:00:00"
```

Vi kan också använda `DateTime.to_string/2` för att konvertera ett datum till en sträng på olika språk baserat på landets inställningar. Till exempel, om vi vill ha datumet i tyska formatet "TT.MM.YYYY", kan vi göra:

```elixir
DateTime.to_string(~U[2020-10-10 12:00:00], "DD.MM.YYYY", format: :long, locale: :de)
```

Output:

```
"10.10.2020"
```

Det finns många andra anpassningsalternativ för att konvertera ett datum till en sträng i Elixir, och du kan läsa mer om dem i dokumentationen för `DateTime.to_string/2` och `DateTime.to_iso8601/1`.

## Djupdykning

När vi använder funktionerna `DateTime.to_iso8601/1` eller `DateTime.to_string/2` för att konvertera datum till strängar, bör vi vara medvetna om tidszoner, eftersom det kan påverka den resulterande strängen. Standardtidszonen i Elixir är UTC, men vi kan ändra den i vår applikation med hjälp av modulen `Timex` eller funktionen `DateTime.from_naive/3`.

Vi måste också vara medvetna om att vi konverterar ett datum till en sträng, vilket inte är reversibelt. Det betyder att vi inte kan konvertera tillbaka en sträng till ett datum utan att hålla reda på det ursprungliga formatet eller inställningarna som användes för att konvertera det till en sträng.

## Se också

- [Elixir Date and Time](https://hexdocs.pm/elixir/1.12/DateTime.html)
- [Elixir Timex](https://hexdocs.pm/timex/Timex.html)
- [ISO 8601 standard](https://www.iso.org/iso-8601-date-and-time-format.html)