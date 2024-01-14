---
title:    "Elixir: Å skrive en tekstfil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor skrive tekstfiler?

Det kan virke unødvendig å skrive en tekstfil i koden din når du allerede har alle funksjonene og variablene du trenger i programmet ditt. Men å skrive en tekstfil kan være nyttig for å lagre informasjon eller kommunisere med andre programmer. Det er også en god måte å sikkerhetskopiere data på.

## Slik gjør du det i Elixir

For å skrive en tekstfil i Elixir trenger vi bare å bruke `File.write/2` funksjonen og gi den to argumenter: filnavnet og innholdet vi ønsker å skrive. Her er et enkelt eksempel:

```Elixir
File.write("min_fil.txt", "Hei, verden!")
```

Dette vil opprette en tekstfil med navnet "min_fil.txt" og skrive teksten "Hei, verden!" til den. Vi kan også bruke variabler eller funksjoner for å skrive variabel tekst eller informasjon fra vårt program.

```Elixir
tekst = "Dette er en tekst"
farge = "rød"

File.write("min_fil.txt", "#{tekst} i en #{farge} tekstfil")
```

Dette vil skrive teksten "Dette er en tekst i en rød tekstfil" til filen vår. Og med funksjoner kan vi til og med skrive data fra vår Elixir applikasjon til en tekstfil for senere bruk eller analyse.

## Dypdykk

Når vi åpner en tekstfil, vil vi se at innholdet vil bli skrevet på en enkel måte, uten noen formatering eller struktur. Vi kan imidlertid bruke Markdown i våre tekstfiler for å organisere og formatere innholdet vårt. Dette gjør det enklere å lese og forstå for både mennesker og andre programmer som skal lese filen.

I tillegg kan vi også bruke `File.stream!/3` funksjonen for å skrive store mengder data til en tekstfil. Dette vil sørge for at vår program ikke bruker for mye minne og vil skrive dataene som en strøm istedenfor samlet.

## Se også

- Elixir Official Documentation: https://hexdocs.pm/elixir/
- Understanding Elixir File I/O: https://dev.to/bohachevsky/understanding-elixir-file-i-o-2haf
- Writing and Reading Text Files in Elixir: https://pragmaticstudio.com/tutorials/writing-and-reading-files-in-elixir