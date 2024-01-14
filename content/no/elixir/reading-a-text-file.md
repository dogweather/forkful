---
title:                "Elixir: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en utvikler som jobber med Elixir, kan du noen ganger støte på behovet for å lese tekstfiler. Dette kan være for å hente innholdet i en konfigurasjonsfil eller for å analysere store mengder data. Uansett årsak er det viktig å vite hvordan du kan gjøre dette effektivt. I denne bloggposten vil vi utforske hvordan man kan lese tekstfiler i Elixir.

## Hvordan

Å lese en tekstfil i Elixir er enkelt og greit. Vi kan bruke funksjonen `File.read/1` som tar inn filbanen som en parameter og returnerer en liste med linjer fra tekstfilen. La oss si at vi har en tekstfil med navnet "test.txt" som inneholder følgende linjer:

```
Hei, dette er en test!
Denne filen inneholder tekst som vi kan lese.
```

For å lese filen i Elixir, kan vi bruke følgende kode:

```Elixir
filbane = "test.txt"
innhold = File.read(filbane)
IO.inspect innhold
```

Denne koden vil skrive ut følgende:

```
["Hei, dette er en test!", "Denne filen inneholder tekst som vi kan lese."]
```

Som du kan se, returnerer funksjonen en liste med linjene i filen. Vi kan også bruke funksjonen `IO.puts/1` for å skrive ut hver linje i filen på egen linje:

```Elixir
filbane = "test.txt"
innhold = File.read(filbane)

Enum.each(innhold, fn linje ->
  IO.puts linje
end)
```

Dette vil skrive ut følgende:

```
Hei, dette er en test!
Denne filen inneholder tekst som vi kan lese.
```

Nå kan du lese og behandle innholdet i en tekstfil på en enkel måte ved hjelp av Elixir.

## Dypdykk

Når det kommer til å lese tekstfiler, er det et par ting du bør være oppmerksom på. For det første, må du sørge for at filbanen du gir til `File.read/1` funksjonen er korrekt og at filen eksisterer. Ellers vil du få en feilmelding.

Du bør også være oppmerksom på at `File.read/1` funksjonen leser hele filen inn i minnet. Dette kan være et problem hvis filen er veldig stor og du har begrensede ressurser på datamaskinen din. I så fall kan det være lurt å bruke funksjonen `File.stream!/1` som leser filen linje for linje og ikke legger hele innholdet i minnet.

Det er også verdt å nevne at Elixir ikke bare kan lese tekstfiler, men også skrive til dem ved hjelp av funksjonene `File.write/2` og `File.append/2`. Dette åpner opp for mange muligheter når det kommer til å behandle filer i Elixir.

## Se også

- [Elixir dokumentasjon om filbehandling](https://hexdocs.pm/elixir/File.html)
- [En guide til filbehandling i Elixir](https://thoughtbot.com/blog/handling-files-with-elixir)
- [Eksempler på å lese og skrive til tekstfiler i Elixir](https://gist.github.com/martinos/b2affa6a9a9f8d98fa2e)