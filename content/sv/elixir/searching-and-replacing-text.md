---
title:    "Elixir: Söka och ersätta text"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift som många programmerare stöter på i sitt dagliga arbete. Det kan finnas flera anledningar till att man behöver göra detta, såsom att fixa buggar, uppdatera gammal kod eller ändra namn på variabler. Oavsett anledning är det viktigt att kunna söka och ersätta text effektivt för att spara tid och minimera fel.

## Hur man gör

I Elixir finns det flera sätt att söka och ersätta text. Ett vanligt sätt är att använda funktionen `String.replace/3`. Den tar tre argument: den ursprungliga strängen, det som ska ersättas och vad det ska ersättas med. Här är ett exempel:

```elixir
iex> String.replace("Hej världen!", "världen", "Elixir")
"Hej Elixir!"
```

Om man vill göra en sökning som är case-insensitive, det vill säga inte skiljer på stora och små bokstäver, kan man använda funktionen `String.replace/4` istället. Den tar en extra parameter som anger om sökningen ska vara case-insensitive. Här är ett exempel:

```elixir
iex> String.replace("Hello World", "world", "Elixir", case: :insensitive)
"Hello Elixir"
```

Det finns även möjlighet att använda regex, eller reguljära uttryck, för att söka och ersätta text. Då kan man använda funktionen `Regex.replace/4` som tar fyra argument: den ursprungliga strängen, regex-uttrycket, vad det ska ersättas med och en optionsparameter. Här är ett exempel:

```elixir
iex> Regex.replace("Hej123världen!", ~r/\d+/, "Elixir", global: true)
"HejElixirvärlden!"
```

## Djupdykning

Sökning och ersättning av text kan också vara användbart när man arbetar med listor och tupler i Elixir. För att söka igenom en lista kan man använda funktionen `Enum.map/2` tillsammans med funktionen `String.replace/3`. Här är ett exempel där vi ersätter alla förekomster av ordet "världen" i en lista med strängar:

```elixir
iex> lista = ["Hej världen!", "Hello world!", "Bonjour le monde!"]

iex> Enum.map(lista, fn str -> String.replace(str, "världen", "Elixir") end)
["Hej Elixir!", "Hello world!", "Bonjour le monde!"]
```

Man kan också använda funktionen `Enum.map/2` för att söka igenom tupler och ersätta text där. Här är ett exempel där vi ersätter ett värde i en tuple med hjälp av regex:

```elixir
iex> tuple = {:namn, "Bruce Wayne"}

iex> Enum.map(tuple, fn {key, value} -> {key, Regex.replace(value, ~r/ /, "_")} end)
{n, "Bruce_Wayne"}
```

## Se även

- [Elixir Dokumentation för String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir Dokumentation för Regex.replace/4](https://hexdocs.pm/elixir/Regex.html#replace/4)
- [Elixir Dokumentation för Enum.map/2](https://hexdocs.pm/elixir/Enum.html#map/2)