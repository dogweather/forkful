---
title:                "Lage en midlertidig fil"
html_title:           "Gleam: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Når vi som programmerere arbeider med å lagre eller manipulere dataene våre, kan vi noen ganger ha behov for å midlertidig lagre informasjonen i en fil. Dette kan være for å sikkerhetskopiere dataene våre, eller for å midlertidig lagre resultatene av en operasjon.

# Slik gjør du det:
For å opprette en midlertidig fil i Gleam, kan vi bruke funksjonen `temporary_file.create`. Denne funksjonen tar imot en rekke med filbanen og navnet på filen vi ønsker å opprette. Etter at filen er opprettet, kan vi skrive data til den ved å bruke funksjonen `write`, og deretter lukke den ved å bruke funksjonen `close`.

```gleam
let {Ok, file} = temporary_file.create("/tmp/", "example")
Ok = write(file, "Dette er et eksempel")
close(file)
```

# Dypdykk:
Å opprette en midlertidig fil for å lagre data er en vanlig praksis blant programmere. Det gir oss en måte å sikkerhetskopiere dataene våre på, og gjør det enklere å håndtere store datamengder. En alternativ tilnærming ville være å lagre dataene direkte i minnet, men dette kan være risikabelt dersom det skulle oppstå strømbrudd eller andre feil.

Ved å bruke funksjonen `temporary_file.create`, vil Gleam opprette en unik filnavn for filen vi ønsker å opprette, slik at vi ikke trenger å bekymre oss for å overskrive eksisterende filer. Dette gjør prosessen mer sikker.

# Se også:
- [Gleam sin offisielle dokumentasjon](https://gleam.run/documentation/)
- [En guide til å lagre data i Gleam](https://medium.com/@jasonshope255/how-to-store-data-in-gleam-from-elixir-7b4084484bc1)