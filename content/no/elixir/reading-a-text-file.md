---
title:                "Leser en tekstfil"
html_title:           "Elixir: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å lese en tekstfil er å få tilgang til informasjon som er lagret i en tekstformatert fil på datamaskinen din. Dette er en vanlig oppgave for programmerere når de trenger å lese og behandle store datamengder.

## Hvordan:
En av måtene å lese en tekstfil i Elixir er ved hjelp av File-modulen. Først må du åpne filen ved hjelp av `File.open/2`-funksjonen og angi filbanen og lesetilgang som parameter. Deretter kan du bruke `IO.read/2`-funksjonen til å lese innholdet i filen. Her er et eksempel på hvordan dette kan gjøres:
```Elixir
{:ok, file} = File.open("tekstfil.txt", [:read])
IO.read(file, :all) # dette vil returnere hele innholdet i tekstfilen
```

## Dykk dypere:
Lesing av tekstfiler har blitt en mye enklere oppgave i Elixir enn det var tidligere. Før i tiden måtte man bruke IO.devices-modulen og sette opp buffere manuelt for å utføre denne oppgaven. Men nå kan man enkelt bruke File-modulen til både å åpne og lese filer. Et alternativ til File-modulen er å bruke IO.binstream-modulen, som spesifikt er designet for å lese binære filer.

## Se også:
For mer informasjon om å lese tekstfiler i Elixir, kan du sjekke ut Elixir-dokumentasjonen på File-modulen: https://hexdocs.pm/elixir/File.html