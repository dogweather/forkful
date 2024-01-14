---
title:    "Gleam: Sammenstilling av strenger"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Vi har alle vært der - å prøve å slå sammen strenger i koden vår og slite med det. Men hvorfor bry seg med å konkatere strenger i det hele tatt? Det viser seg at det kan være en ganske nyttig funksjon!

# Hvordan

La oss se på noen eksempler på hvordan vi kan konkatere strenger i Gleam:

```gleam
let navn = "Sara"
let hilsen = "Hei " ++ navn
```

Her ser vi at vi har definert en variabel kalt "navn" og deretter brukt konkateneringsoperatøren "++" for å legge til innholdet i variabelen til en annen streng. Dette vil gi oss en ny variabel "hilsen" med verdien "Hei Sara".

```gleam
let tall = 42
let streng = "Tallet er " ++ to_string(tall)
```

I dette tilfellet ser vi også at vi kan konkatere strenger med tall. Vi bruker funksjonen "to_string" for å konvertere tallet til en streng før vi konkatenerer det med resten av teksten.

# Dykk dypere

Det er viktig å merke seg at konkatenering er en langsommere operasjon enn å bruke formateringsstrenger. Dette skyldes at hver gang en konkateneringsoperasjon utføres, må det opprettes en ny streng og kopieres innholdet fra de to originale strengene. Dette kan føre til at koden vår blir tregere og mindre effektiv.

Det er også verdt å merke seg at Gleam har en innebygd funksjon for å konkatere flere strenger på en mer effektiv måte - "String.concat". Denne funksjonen tar inn en liste med strenger og kombinerer dem til én streng.

# Se også

- [Gleam dokumentasjon for Strings](https://gleam.run/core/string.html)
- [Video om strenger i Gleam](https://www.youtube.com/watch?v=6-km0Y8vPbg)
- [Eksempelkode for å utforske konkatenering i Gleam](https://github.com/search?p=2&q=gleam+string&type=Code)

*Takk for at du leste! Håper dette hjelper deg med å forstå konkatenering i Gleam.*