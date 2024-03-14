---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:58.877401-07:00
description: "Assosiative arrayer, eller hash-maps, lar deg lagre data som n\xF8kkel-verdipar,\
  \ noe som gj\xF8r det enklere \xE5 organisere og hente informasjon via n\xF8kkel.\
  \ De er\u2026"
lastmod: '2024-03-13T22:44:41.217929-06:00'
model: gpt-4-0125-preview
summary: "Assosiative arrayer, eller hash-maps, lar deg lagre data som n\xF8kkel-verdipar,\
  \ noe som gj\xF8r det enklere \xE5 organisere og hente informasjon via n\xF8kkel.\
  \ De er\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative arrayer, eller hash-maps, lar deg lagre data som nøkkel-verdipar, noe som gjør det enklere å organisere og hente informasjon via nøkkel. De er hendige når du trenger en mer strukturert måte å håndtere data på enn bare lister, spesielt i konfigurasjoner og når du håndterer et spekter av attributter.

## Hvordan:

Fish støtter ikke assosiative arrayer naturlig som Bash 4+, men du kan oppnå lignende funksjonalitet ved å bruke en kombinasjon av lister og strengmanipulasjon. Her er hvordan du etterligner dem:

Først, setter du opp "assosiativt array"-elementer separat:

```Fish Shell
set food_color_apple "rød"
set food_color_banana "gul"
```

For å få tilgang til et element, bare referer direkte til det:

```Fish Shell
echo $food_color_apple
# Utdata: rød
```

Hvis du trenger å iterere over dem, bruk en for-løkke som vurderer en navnekonvensjon:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Utdata:
# rød
# gul
```

For de som savner Bashes `${!array[@]}` for å få alle nøkler, kan du lagre nøkler i en separat liste:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'er' $food_color_$key
end
# Utdata:
# apple er rød
# banana er gul
```

## Dypdykk

Ekte assosiative arrayer som i andre skriptspråk er ennå ikke en del av Fish sin tilnærming. Fiksen som vises nytter Fish sin strengmanipulasjon og listekapasiteter for å skape en pseudo-assosiativ array-struktur. Selv om det fungerer, er det ikke like rent eller feilfritt som innebygd støtte for assosiativ array ville vært. Andre skall som Bash og Zsh tilbyr innebygd funksjonalitet for assosiative arrayer, noe som resulterer i mer rettfram, lesbart kode. Imidlertid sikter Fish sitt designprinsipp mot enkelhet og brukervennlighet, muligens på bekostning av slike funksjoner. Fiksen dekker de fleste behov, men hold et øye med utviklingen av Fish Shell - utviklerne forbedrer aktivt og legger til funksjoner basert på tilbakemeldinger fra samfunnet.
