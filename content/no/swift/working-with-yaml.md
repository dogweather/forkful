---
title:                "Arbeid med yaml"
html_title:           "Swift: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med YAML kan være en nyttig ferdighet for alle som er interessert i å jobbe med data og konfigurasjonsfiler. Det å kunne formatere og lagre data på en strukturert måte kan gjøre det enklere å organisere og modifisere informasjon.

## Slik gjør du det

YAML er et tekstbasert filformat som brukes til å representere strukturerte data på en lesbar måte. Det kan brukes til å konfigurere applikasjoner, lage metadata eller lagre data for programmeringsspråk som Swift. For å begynne å jobbe med YAML, følg disse enkle stegene:

1. Installer en teksteditor som støtter YAML-formatet, som for eksempel Visual Studio Code, Sublime Text eller Atom.
2. Lag en ny YAML-fil med filtypen ".yml" eller ".yaml".
3. Skriv inn ønsket datastruktur basert på YAML-syntaksen (se nedenfor for eksempler).
4. Lagre filen og du er klar til å bruke YAML i ditt prosjekt!

Et eksempel på en grunnleggende YAML-fil:

```
# Kommentarer skrives med hashtag foran
navn: Ylva
alder: 25
hobbyer:
  - fotografering
  - streaming
  - reise
```

I dette eksemplet er "navn" og "alder" nøkkelverdier, mens "hobbyer" er en underliste av flere elementer.

## Dykk dypere

Når du har blitt mer komfortabel med YAML-syntaksen, kan du utforske mer avanserte konsepter som inkluderer flerlagsstrukturer, referanser og inkludering av filer. Du kan også bruke YAML til å definere dataklasser og objekter i ditt Swift-prosjekt ved hjelp av tredjepartsbiblioteker som "Yams" eller innebygde funksjoner som "Codable".

Et eksempel på en mer avansert YAML-fil:

```
# Produktkatalog
produkter:
  - navn: Skjorte
    beskrivelse: En enkel, men elegant skjorte
    pris: 300
    tilgjengelige størrelser:
      - S
      - M
      - L
  - navn: Bukse
    beskrivelse: Komfortabel og stilfull bukse
    pris: 450
    tilgjengelige størrelser:
      - S
      - M
      - L
  - navn: Kjole
    beskrivelse: Fantastisk festkjole
    pris: 700
    tilgjengelige størrelser:
      - XS
      - S
      - M
```

Her bruker vi flerlagsstruktur for å organisere produktdata og inkluderer også en underliste for å vise tilgjengelige størrelser for hvert produkt.

## Se også

* [YAML-spesifikasjon](https://yaml.org/spec/1.2/spec.html)
* [Yams - Swift YAML-parser](https://github.com/jpsim/Yams)
* [Codable - innebygd Swift-funksjon for å konvertere dataklasser til og fra YAML](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html#ID566)