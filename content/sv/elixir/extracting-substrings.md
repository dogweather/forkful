---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrahering av delsträngar i Elixir

## Vad & Varför?
Att extrahera delsträngar innebär att hitta och återge specifika delar av en större sträng. Programmerare gör detta för att manipulera, söka efter eller återanvända dessa specifika delbitar.

## Så här gör du:
I Elixir kan du extrahera substrings med hjälp av `String.slice/2`. Här är ett exempel:

```Elixir
str = "Hej, välkommen till Elixir programmering!"
IO.puts String.slice(str, 0..2)
```

I detta fall skulle output bli:

```
"Hej"
```

## Djupdykning
Elixir, utvecklad 2011, har många funktioner som är hämtade från Erlang och andra funktionella programmeringsspråk. Funktionen `String.slice/2` är en sådan, och den gör det enkelt att plocka ut delsträngar baserat på index.

Alternativt kan du använda `binary_part/3` för mer specifik kontroll över byte-storleken:

```Elixir
:binary.part("Hej, välkommen till Elixir programmering!", {0, 3})
```

För att implementera detta är det viktigt att förstå att Elixir, precis som Erlang, behandlar strängar som binärdata. Därför kan du ha direktåtkomst och manipulera strängar på byte-nivå.

## Se även
- [`String.slice/2` documentation](https://hexdocs.pm/elixir/String.html#slice/2) för mer information om hur man använder denna funktion.
- [`binary_part/3 documentation`](http://erlang.org/doc/man/binary.html#part-3) om du vill ha mer kontroll över storleken på delsträngarna.
- [`:binary` module](http://erlang.org/doc/man/binary.html) för allmän information om hur Elixir hanterar binärdata och strängar.