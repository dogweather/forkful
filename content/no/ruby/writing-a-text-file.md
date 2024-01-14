---
title:    "Ruby: Skriver en tekstfil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en grunnleggende ferdighet i Ruby-programmering. Det gjør det mulig å lagre og lese data på en enkel måte, og er veldig nyttig for å lagre og dele informasjon.

## Slik gjør du det

For å skrive en tekstfil i Ruby, må du følge disse trinnene:

1. Åpne en ny tekstfil ved å bruke `File.new`.
2. Skriv inn ønsket data ved hjelp av `puts`-kommandoen.
3. Lukk filen ved å bruke `close`-metoden.

```Ruby
file = File.new("min_tekstfil.txt", "w")
file.puts("Dette er en tekstfil som er laget med Ruby.")
file.close
```

Dette vil opprette en tekstfil med navnet "min_tekstfil.txt" og skrive inn teksten i den. Den vil da bli lukket, og filen vil være klar til å bli lest.

## Dypdykk

Nå som du vet hvordan du skal skrive en tekstfil, kan du utforske mer avanserte funksjoner som å lese og skrive til forskjellige deler av en fil, håndtere feil og bruke forskjellige moduser for å åpne en fil.

For å lese en tekstfil, kan du bruke `File.read`-kommandoen, som vil hente all teksten fra filen og lagre den som en streng.

```Ruby
file = File.new("min_tekstfil.txt", "r")
puts File.read(file)
file.close
```

Du kan også bruke `File.foreach`-kommandoen for å lese en fil linje for linje, eller `File.readlines` for å lese alle linjene inn i et array.

Se dokumentasjonen for mer informasjon om forskjellige metoder for å lese og skrive til tekstfiler i Ruby.

## Se også

- [Ruby dokumentasjon for åpning av filer](https://ruby-doc.org/core-3.0.0/File.html)
- [Tutorialspoint for å skrive til en fil i Ruby](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Youtube: Ruby Tutorial: Working with Files](https://www.youtube.com/watch?v=CWvsLV3XYhI)