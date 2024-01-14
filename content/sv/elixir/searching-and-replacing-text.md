---
title:                "Elixir: Sökning och ersättning av text"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en grundläggande uppgift inom programmering. Det kan hjälpa till att effektivisera arbetsflödet och göra det lättare att hantera data på ett enhetligt sätt.

## Hur man gör det

För att söka och ersätta text i Elixir använder vi funktionen `String.replace/3`. Här är ett enkelt exempel på hur vi kan ersätta en sträng i en variabel:

```elixir
sträng = "Hej världen"
String.replace(sträng, "världen", "värld")
```

Detta kommer att ersätta "världen" med "värld" i vår strängvariabel och returnera "Hej värld".

Det finns också möjlighet att använda reguljära uttryck i sök- och ersättningsprocessen. Till exempel, om vi vill ersätta alla siffror i en sträng med stjärnor, kan vi använda följande kod:

```elixir
sträng = "Det finns 12345 stjärnor på himlen"
Regex.replace(~r/\d+/, sträng, "*")
```

Detta kommer att returnera "Det finns ***** stjärnor på himlen".

## Djupdykning

Förutom de enkla fallen som vi har sett ovan, finns det många andra sätt att söka och ersätta text i Elixir. Vi kan till exempel använda oss av options för att specificera hur vi vill att sökningen ska utföras, såsom att ignorera skiftlägeskänslighet eller att endast göra ersättningar på vissa positioner.

Vi kan också använda funktionen `String.replace_all/3` för att göra flera ersättningar samtidigt i en sträng. Denna funktion tar emot en lista med tuple av formen `{"söksträng", "ersättning"}` och applicerar alla ersättningar i en enda sökning.

## Se även

- [Elixir dokumentation: String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir dokumentation: String.replace_all/3](https://hexdocs.pm/elixir/String.html#replace_all/3)
- [Regular expressions in Elixir](https://www.amberbit.com/blog/2017/10/16/regular-expressions-in-elixir/)