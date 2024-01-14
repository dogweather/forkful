---
title:    "Ruby: Sammenføyning av strenger"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

I Ruby, som i mange andre programmeringsspråk, er det ofte nødvendig å kombinere eller sette sammen flere strenger av tekst. Dette kan være for å lage en komplett setning eller for å bygge en kompleksere datastruktur. Uansett årsak, er sammenslåing av strengestrukturene en vanlig oppgave i programmering, og derfor er det viktig å lære hvordan det gjøres.

## Hvordan

Det å sette sammen strenger i Ruby er enkelt og intuitivt. Det kan gjøres ved å bruke en spesiell operator, kalt "concatenation operator", som består av to pluss-symboler (```+```). La oss ta en titt på et eksempel:

```Ruby
navn = "Jenny"
alder = 26
puts "Hei, mitt navn er " + navn + " og jeg er " + alder.to_s + " år gammel."
```

Dette vil gi følgende utskrift:

```
Hei, mitt navn er Jenny og jeg er 26 år gammel.
```

Her legger vi sammen tre forskjellige strenger: "Hei, mitt navn er ", navnet som er lagret i variabelen ```navn```, og til slutt " og jeg er " fulgt av alderen, som er konvertert fra en ```Integer``` til en ```String``` ved hjelp av ```.to_s``` metoden.

## Dypdykk

For å forstå hvordan sammenslåing av strenger fungerer i Ruby, er det viktig å vite at en streng i seg selv er en objekttype. Dette betyr at vi kan bruke forskjellige metoder og operasjoner på strenger for å endre eller manipulere dem.

En streng i Ruby er egentlig en samling av tegn, hvor hvert tegn har en numerisk verdi kalt en "ASCII code". Når vi bruker concatenation operator på strenger, vil Ruby se på hver enkelt karakter og legge dem sammen i riktig rekkefølge for å danne en ny streng.

En annen måte å sammenslå strenger på er ved hjelp av metoden ```.concat```. Denne metoden tar inn en annen streng og legger den til på slutten av den første strengen. Dette gjør det mulig å kombinere flere strenger på en enklere måte. La oss se på et eksempel:

```Ruby
navn = "Jenny"
etternavn = " Olsen"
navn.concat(etternavn)
```

Dette vil gi oss strengen "Jenny Olsen" som output. Her brukes metoden ```.concat``` til å legge til etternavnet på slutten av navnet.

## Se også

- [Ruby-dokumentasjon: Strenger](https://www.ruby-lang.org/no/documentation/quickstart/3/)
- [Concatenating Strings in Ruby](https://www.techotopia.com/index.php/Concatenating_Strings_in_Ruby)