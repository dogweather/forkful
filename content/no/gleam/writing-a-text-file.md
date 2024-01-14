---
title:    "Gleam: Å skrive en tekstfil"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en viktig del av programmering, spesielt når man jobber med Gleam. Det lar deg lagre og organisere data på en strukturert måte, som deretter kan brukes av programmet ditt.

## Hvordan

For å skrive en tekstfil i Gleam, bruker du funksjonen `File.write`, som tar inn to parametere: stien til filen du vil skrive til og innholdet du vil inkludere i filen. For å sikre at filen blir lukket riktig etter skriving, kan man bruke `File.with_open`.

La oss se på et eksempel hvor vi lagrer navn og aldre i en tekstfil:

```Gleam
import File

let people = [("Jens", 28), ("Sarah", 33), ("Lisa", 24)]
let file_path = "people.txt"

File.with_open(file_path, fn(handle) ->
  for person in people do
    let (name, age) = person
    let text = "Navn: " ++ name ++ ", Alder: " ++ String.to_int(age) ++ "\n"
    File.write(handle, text)
  end
end)
```

Etter å ha kjørt dette programmet, vil du finne en fil som heter "people.txt" i samme mappe som programmet ditt. Denne filen vil inneholde følgende:

```
Navn: Jens, Alder: 28
Navn: Sarah, Alder: 33
Navn: Lisa, Alder: 24
```

## Deep Dive

Når du skal skrive en tekstfil, er det viktig å være oppmerksom på hvordan du formaterer teksten din. En vanlig måte å gjøre dette på er ved å bruke "escaped characters", som for eksempel `\n` for å lage en ny linje.

I tillegg er det viktig å sørge for at filen lukkes riktig etter skriving. Dette kan være en kilde til feil og problemer hvis det ikke gjøres riktig.

## Se Også

- Gleam sin offisielle dokumentasjon for `File`-modulen: https://gleam.run/packages/gleam_stdlib/File.html 
- En grundig gjennomgang av filbehandling i Gleam: https://medium.com/@ardenrickel/how-to-handle-file-input-output-in-gleam-ecd1dcc6e829 
- Et annet eksempel på å skrive en tekstfil i Gleam: https://gist.github.com/taylorwood/cd115a66aa8d3c3312e3c05cf5142fe3