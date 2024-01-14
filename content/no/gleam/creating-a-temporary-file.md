---
title:    "Gleam: Opprette en midlertidig fil"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å opprette midlertidige filer kan være nyttig når du jobber med Gleam programmeringsspråket. Midlertidige filer kan for eksempel brukes for å lagre midlertidig data mens du jobber med et prosjekt, eller for å teste forskjellige deler av koden din uten å endre den originale koden. 

## Hvordan

For å opprette en midlertidig fil i Gleam, kan du bruke følgende kode:

```Gleam
let tmp_file = File.temporary()
// tmp_file inneholder nå en midlertidig fil
```

Du kan også velge å gi filen et navn og en filtype ved å endre på koden, som vist under:

```Gleam
let tmp_file = File.temporary("mitt_navn.txt")
// tmp_file inneholder nå en midlertidig fil med navnet "mitt_navn.txt"
```

For å skrive data til den midlertidige filen, kan du bruke `File.write` funksjonen, som vist under:

```Gleam
File.write(tmp_file, "Dette er data som skal skrives til filen")
```

For å lese data fra den midlertidige filen, kan du bruke `File.read` funksjonen, som vist under:

```Gleam
let data = File.read(tmp_file)
// data inneholder nå data som er lest fra den midlertidige filen
```

Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved å bruke `File.delete` funksjonen, som vist under:

```Gleam
File.delete(tmp_file)
// Den midlertidige vil nå bli slettet fra systemet
```

## Dypdykk

Når du oppretter en midlertidig fil i Gleam, blir den lagret i et midlertidig filsystem som er spesifikt for programmet ditt. Dette betyr at filen bare vil være tilgjengelig mens programmet kjører. Når programmet avsluttes, vil den midlertidige filen bli slettet automatisk.

Det er viktig å huske på at midlertidige filer bare bør brukes for midlertidig lagring av data. De bør ikke brukes som en permanent løsning for å lagre viktig informasjon eller data.

## Se Altså

- [Gleam offisiell dokumentasjon om midlertidige filer](https://gleam.run/articles/temporary-files/)
- [En tutorial om midlertidige filer i Gleam](https://smockle.com/en/resources/creating-temporary-files-in-gleam)