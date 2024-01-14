---
title:                "Elixir: Å lese en tekstfil"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor vil noen lese en tekstfil? Det kan være mange grunner til dette, enten det er for å hente ut informasjon, behandle data, eller bare for å lære.

Å kunne lese en tekstfil er en grunnleggende ferdighet som kan være nyttig for mange programmeringsspråk, inkludert Elixir. I denne bloggposten vil vi utforske hvordan vi kan lese tekstfiler ved hjelp av Elixir.

## Hvordan
For å lese en tekstfil i Elixir, trenger vi å bruke funksjonen `File.read/1`. Denne funksjonen tar inn filbanen som en argument, og returnerer innholdet av filen som en streng. La oss se på et eksempel:

```
Elixir
content = File.read("min_fil.txt")
IO.puts(content)
```

Her har vi en tekstfil som heter `min_fil.txt`, som inneholder tekstlinjer. Når vi kjører koden, vil `content` variabelen inneholde innholdet av filen som en streng, og deretter skriver vi ut denne strengen ved hjelp av `IO.puts` funksjonen.

Vi kan også lese filen linje for linje ved hjelp av `File.stream!/1` funksjonen. Denne funksjonen returnerer en strøm av linjer fra filen, som vi kan behandle videre. La oss se på et eksempel:

```
Elixir
lines = File.stream!("min_fil.txt")
Enum.each(lines, fn line ->
  IO.puts(line)
end)
```

Her har vi brukt `Enum.each` funksjonen til å gå gjennom hver linje i filen og bruke `IO.puts` for å skrive den ut. Dette er nyttig hvis vi ikke ønsker å laste hele filen inn i minnet på en gang.

## Dypdykk
Når vi leser en tekstfil, er det viktig å huske på at det kan være forskjellige tegnsett som brukes i filen. For å unngå eventuelle problemer med tegnsett, kan vi bruke `File.read!/1` og `File.stream!/1` funksjonene og angi ønsket tegnsett som et argument.

Vi kan også lese og behandle data i strukturert format som JSON eller CSV ved hjelp av tilleggsbiblioteker som `Poison` eller `CSV`. Disse bibliotekene gjør det enkelt å lese og behandle slike filformater.

## Se også
- [Elixir offisiell dokumentasjon](https://hexdocs.pm/elixir/File.html#read/1)
- [Poison bibliotek](https://hexdocs.pm/poison/1.2.0/overview.html)
- [CSV bibliotek](https://hexdocs.pm/csv/CSV.html)

Med disse kunnskapene bør du nå være klar til å lese tekstfiler i Elixir. Ha det gøy med programmering!