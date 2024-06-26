---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:57.834208-07:00
description: "Hoe: In Bash is de `$RANDOM` variabele de go-to voor het genereren van\
  \ willekeurige getallen. Elke keer dat je het referereert, biedt Bash een pseudo-\u2026"
lastmod: '2024-03-13T22:44:50.975528-06:00'
model: gpt-4-0125-preview
summary: In Bash is de `$RANDOM` variabele de go-to voor het genereren van willekeurige
  getallen.
title: Willekeurige getallen genereren
weight: 12
---

## Hoe:
In Bash is de `$RANDOM` variabele de go-to voor het genereren van willekeurige getallen. Elke keer dat je het referereert, biedt Bash een pseudo-willekeurig geheel getal tussen 0 en 32767. Laten we enkele praktische voorbeelden verkennen:

```Bash
# Basisgebruik van $RANDOM
echo $RANDOM

# Een willekeurig getal genereren in een gespecificeerd bereik (hier 0-99)
echo $(( RANDOM % 100 ))

# Een "veiliger" willekeurig getal genereren, geschikt voor wachtwoorden of sleutels
# Gebruiken van /dev/urandom met od commando
head -c 8 /dev/urandom | od -An -tu4

# RANDOM zaaien voor reproduceerbaarheid
RANDOM=42; echo $RANDOM
```

Voorbeelduitkomst (let op: de feitelijke uitkomst zal variëren aangezien de getallen willekeurig zijn):
```Bash
16253
83
3581760565
17220
```

## Diepere Duik
Het mechanisme achter Bash's `$RANDOM` genereert pseudo-willekeurige getallen, wat betekent dat ze een algoritme volgen en in theorie voorspelbaar kunnen zijn - een potentieel veiligheidsprobleem voor applicaties die echte onvoorspelbaarheid vereisen. Moderne cryptografische applicaties vereisen meestal willekeur die is afgeleid van fysieke fenomenen of van hardware die specifiek ontworpen is om willekeurige gegevens te genereren, zoals `/dev/urandom` of `/dev/random` in Linux, die omgevingslawaai verzamelen.

Voor informele of niet-beveiligingskritieke taken voldoet `$RANDOM` en biedt het voordeel van eenvoud. Echter, voor cryptografische doeleinden of waar de kwaliteit van willekeurigheid kritisch is, zouden ontwikkelaars naar andere tools en talen moeten kijken die met cryptografie in gedachten zijn ontworpen, zoals OpenSSL of programmeertalen met robuuste bibliotheken voor het genereren van willekeurige getallen.

Hoewel Bash's `$RANDOM` zijn doel dient in scripts die basis willekeurige getallen vereisen, zouden zijn beperkingen ontwikkelaars moeten sturen naar robuustere oplossingen voor applicaties waar de kwaliteit of veiligheid van de willekeurigheid van belang is.
