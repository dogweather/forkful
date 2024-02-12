---
title:                "Willekeurige getallen genereren"
aliases:
- /nl/swift/generating-random-numbers/
date:                  2024-01-28T22:01:10.866748-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmeren gaat over het creëren van niet-deterministische of onvoorspelbare numerieke waarden. Programmeurs gebruiken willekeurige getallen om verschillende redenen, zoals het simuleren van onvoorspelbaarheid in spellen, het selecteren van willekeurige monsters uit gegevensverzamelingen, of voor cryptografische doeleinden.

## Hoe te:

Swift biedt een eenvoudige manier om willekeurige getallen te genereren via zijn standaardbibliotheek. Hier is hoe je het doet voor verschillende numerieke typen:

```Swift
// Genereer een willekeurig geheel getal tussen 0 en Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Genereer een willekeurig zwevend kommagetal tussen 0.0 en 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Genereer een willekeurige Bool-waarde
let randomBool = Bool.random()
print(randomBool)
```

Voorbeelduitvoer kan variëren omdat, nou ja, we tenslotte met willekeurigheid te maken hebben. Het meerdere keren uitvoeren van de code zal verschillende getallen en booleaanse waarden opleveren.

## Diepere Duik

Swift's benadering van willekeurige getallengeneratie is gebouwd op een robuuste en efficiënte pseudo-willekeurige getallengenerator (PRNG). Voor Swift 4.2 waren ontwikkelaars afhankelijk van externe bibliotheken of de onderliggende platformcapaciteiten, wat kon leiden tot inconsistenties tussen verschillende platforms en omgevingen. Met de introductie van native API's in Swift 4.2 werd het genereren van willekeurige getallen zowel eenvoudiger als consistenter, ongeacht het onderliggende platform.

Het is echter cruciaal om te begrijpen dat de standaard willekeurige getallengenerator in Swift niet geschikt is voor cryptografische doeleinden. Voor cryptografie moeten ontwikkelaars het `Security` framework op Apple-platforms gebruiken, dat toegang biedt tot cryptografisch beveiligde willekeurige bytes. Voor zover mij bekend, bevat Swift geen platformonafhankelijke cryptografische willekeurige getallengenerator in zijn standaardbibliotheek, waardoor ontwikkelaars op niet-Apple platforms genoodzaakt zijn om voor dergelijke behoeften naar externe bibliotheken te zoeken.

In het rijk van wetenschappelijk rekenen of situaties die een deterministische reeks van pseudo-willekeurige getallen vereisen (waarbij de reeks exact gereproduceerd kan worden), is Swift’s willekeurige getallengeneratie mogelijk niet de beste keus zonder de mogelijkheid om de generator te zaaien. In dergelijke gevallen worden vaak gespecialiseerde bibliotheken en algoritmen ingezet om aan deze precieze vereisten te voldoen.
