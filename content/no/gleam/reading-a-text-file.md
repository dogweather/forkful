---
title:                "Ans: Lese en tekstfil"
html_title:           "Gleam: Ans: Lese en tekstfil"
simple_title:         "Ans: Lese en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Lesing av en tekstfil er en vanlig oppgave for programmerere. Det rett og slett betyr å åpne en fil og lese innholdet i den. Dette er nyttig for å hente data fra en fil og bruke den i koden din.

## Slik gjør du det:

```Gleam
// Åpne en fil i skrive- eller lesemodus
File.open("tekstfil.txt", write) do |fil|
  // Les linje for linje og skriv ut innholdet
  for line in fil.each_line() do
    println(line)
  end
end
```

```Gleam
// Opprett en fil og skriv inn noe tekst
File.open("ny_fil.txt", write) do |fil|
  fil.write("Dette er en ny fil!")
end
```

## Dykk dypere:

Å lese tekstfiler er en viktig del av programmering, spesielt når man jobber med data fra eksterne kilder. Filer med tekstformat brukes ofte til å lagre og transportere data. Alternativer til å lese tekstfiler inkluderer å lese og skrive til en database eller å bruke API-er for å få tilgang til eksterne datakilder.

For å lese en tekstfil i Gleam benytter man seg av det innebygde File-modulen. Denne inneholder metoder for å åpne, lese og skrive til filer. Det kan også være lurt å håndtere eventuelle feil som kan oppstå under lesing av en fil. 

## Se også:

- [Gleam sin offisielle dokumentasjon om File-modulen](https://gleam.run/book/stdlib#the-file-module)
- [En artikkel om å lese og skrive til filer i Java](https://docs.oracle.com/javase/tutorial/essential/io/file.html)