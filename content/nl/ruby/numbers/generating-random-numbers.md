---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:55.912776-07:00
description: 'Hoe: Ruby biedt verschillende methoden om willekeurige getallen te genereren,
  voornamelijk via de `Random` klasse.'
lastmod: '2024-04-05T21:53:51.341725-06:00'
model: gpt-4-0125-preview
summary: Ruby biedt verschillende methoden om willekeurige getallen te genereren,
  voornamelijk via de `Random` klasse.
title: Willekeurige getallen genereren
weight: 12
---

## Hoe:
Ruby biedt verschillende methoden om willekeurige getallen te genereren, voornamelijk via de `Random` klasse.

### Basis Willekeurig Getal
Om een basis willekeurig getal te genereren:

```Ruby
puts rand(10) # Genereert een willekeurig getal tussen 0 en 9
```

### Willekeurig Getal Binnen een Bereik
Voor een willekeurig getal binnen een specifiek bereik:

```Ruby
puts rand(1..10) # Genereert een willekeurig getal tussen 1 en 10
```

### Gebruik van de Random Klasse
Om een herhaalbare reeks van willekeurige getallen te creëren, kun je de `Random` klasse gebruiken met een zaadwaarde.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Genereert een voorspelbaar "willekeurig" getal
```

### Genereren van een Willekeurig Element uit een Array
Selecteer een willekeurig element uit een array:

```Ruby
colors = ["rood", "blauw", "groen", "geel"]
puts colors.sample # Selecteert willekeurig een element uit de array
```

### Voorbeelduitvoer:
Elk codefragment hierboven zal, wanneer uitgevoerd, verschillende uitvoeren produceren vanwege hun willekeurige aard. Zo kan `rand(10)` bijvoorbeeld `7`uitvoeren, terwijl `colors.sample` mogelijk `"groen"` uitvoert.

## Diepgaand
Het concept van het genereren van willekeurige getallen in de informatica is paradoxaal, omdat computers deterministische instructies volgen. Vroege methoden waren sterk afhankelijk van externe input om onvoorspelbaarheid te bereiken. Ruby's willekeurigheid is gebouwd op het Mersenne Twister-algoritme, een pseudo-willekeurige getallengenerator bekend om zijn grote periode en uniforme verdeling, waardoor het zeer geschikt is voor toepassingen die hoogwaardige willekeurigheid vereisen.

Hoewel de ingebouwde methoden van Ruby voor de meeste behoeften goed volstaan, zijn ze mogelijk niet voldoende voor alle cryptografische doeleinden, aangezien de voorspelbaarheid van pseudo-willekeurige getallen een kwetsbaarheid kan zijn. Voor cryptografische beveiliging zouden Ruby-ontwikkelaars bibliotheken zoals `OpenSSL::Random` kunnen verkennen, die zijn ontworpen om cryptografisch veilige willekeurige getallen te produceren, waardoor een hogere onvoorspelbaarheid voor gevoelige toepassingen wordt gegarandeerd.
