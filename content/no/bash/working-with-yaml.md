---
title:                "Bash: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en utvikler som jobber med konfigurasjonsfiler, har du sannsynligvis hørt om YAML. YAML (YAML Ain't Markup Language) er et format for å representere data i en menneskeleselig og strukturert form. Men hvorfor bør du lære å jobbe med YAML?

YAML er et populært valg blant utviklere fordi det er enkelt å forstå og skrive. Det inneholder intuitive syntaks og er lett å lese for både mennesker og maskiner. Det er også et åpent format som kan brukes på tvers av ulike programmeringsspråk, noe som gjør det til et fleksibelt valg for konfigurasjonsfiler.

## Hvordan

Hvis du vil begynne å jobbe med YAML, er det første du må gjøre å installere en YAML parser på datamaskinen din. I dette eksempelet vil vi bruke "yq" - en YAML parser for Bash.

Før du begynner å kode, bør du først lese gjennom YAML sitt offisielle dokumentasjon for å bli kjent med syntaksen og strukturen til filene.

La oss si at vi har en YAML-fil som ser slik ut:

```Bash
# test.yml

navn: Johan
alder: 35
hobbyer:
- brettspill
- klatring
```

Vi kan bruke "yq" til å få ut navnet fra denne filen ved å skrive følgende kommando:

```Bash
yq .navn test.yml
```

Dette vil gi oss følgende output:

```Bash
Johan
```

Vi kan også bruke "yq" til å legge til en ny verdi til YAML-filen vår. For eksempel, hvis vi ønsker å legge til en ny hobby, kan vi skrive følgende kommando:

```Bash
yq '.hobbyer += "fotografering"' test.yml
```

Dette vil da legge til "fotografering" som en ny verdi under "hobbyer" i YAML-filen vår.

## Dykk Dypere

YAML støtter også avanserte funksjoner som løkker og betingelser, som kan være nyttig for å lage dynamiske konfigurasjonsfiler. Det er også mulig å inkludere deler av en YAML-fil i en annen ved hjelp av "<<"-operator.

Det finnes også ulike verktøy og biblioteker som kan hjelpe deg med å jobbe med YAML, som for eksempel "yq", "yamllint" og "PyYAML". Ved å utforske disse verktøyene og lære mer om YAML, vil du kunne dra nytte av dette effektive og populære formatet i ditt arbeid som utvikler.

## Se også

- [YAML offisiell dokumentasjon](https://yaml.org/spec/)
- [yq](https://yq.readthedocs.io/en/latest/)
- [yamllint](https://yamllint.readthedocs.io/en/latest/)
- [PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)