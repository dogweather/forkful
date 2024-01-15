---
title:                "Å skrive en tekstfil"
html_title:           "Gleam: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen engasjere seg i å skrive en tekstfil? Vel, det er faktisk et nyttig og grunnleggende konsept innen programmering. Å kunne skrive og lese data fra en fil er viktig for å behandle store mengder informasjon og lagre den for senere bruk.

## Hvordan

Du trenger ikke å være en erfaren programmør for å kunne skrive en tekstfil i Gleam. Det er en ganske enkel prosess som kan gjøres med få linjer med kode. Først må du importere biblioteket "gleam/io". Deretter kan du bruke funksjonen "write_file" for å opprette en tekstfil og skrive innhold i den.

```
import gleam/io

let file = io.write_file("min_tekstfil.txt", "Dette er noen tekster jeg skriver til filen!")
```

## Dykk dypere

Den enkle koden over vil lage en fil ved navn "min_tekstfil.txt" og skrive teksten "Dette er noen tekster jeg skriver til filen!" inn i den. Men det finnes også flere muligheter for å tilpasse og manipulere dataen som skrives til filen.

Du kan for eksempel bruke en "append"-funksjon for å legge til mer tekst i en allerede eksisterende fil, i stedet for å overskrive hele innholdet. Du kan også bruke for-løkker for å gå gjennom et datasett og skrive hver enkelt verdi til en linje i filen. Mulighetene er mange, og det avhenger av hva slags informasjon du ønsker å lagre og hvordan du vil organisere den.

## Se også

- Offisiell dokumentasjon for Gleam: https://gleam.run/
- "Begynn å programmere med Gleam" (på norsk): https://medium.com/@pieterdm/hello-world-i-gleam-d55495bb351b
- "Lær deg å bruke filer i Gleam" (på engelsk): https://dev.to/smizoguchi/lets-learn-how-to-use-files-in-gleam-461i