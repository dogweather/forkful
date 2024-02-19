---
aliases:
- /nl/haskell/using-associative-arrays/
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:38.852432-07:00
description: "Associatieve arrays, of woordenboeken, in Haskell gaan helemaal over\
  \ het koppelen van sleutels aan waarden voor snelle opzoekingen en effici\xEBnt\u2026"
lastmod: 2024-02-18 23:09:01.890862
model: gpt-4-0125-preview
summary: "Associatieve arrays, of woordenboeken, in Haskell gaan helemaal over het\
  \ koppelen van sleutels aan waarden voor snelle opzoekingen en effici\xEBnt\u2026"
title: Gebruik van associatieve arrays
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, of woordenboeken, in Haskell gaan helemaal over het koppelen van sleutels aan waarden voor snelle opzoekingen en efficiënt databeheer. Programmeurs gebruiken ze om collecties van gekoppelde elementen te behandelen, waarbij het zoeken naar een element een fluitje van een cent is, in vergelijking met lijsten.

## Hoe te:

Haskell heeft niet standaard associatieve arrays zoals sommige andere talen, maar het biedt een krachtige standaardbibliotheek genaamd `Data.Map` voor het werken met sleutel-waardeparen. Laten we onze mouwen opstropen en kijken hoe we ze kunnen gebruiken!

Zorg eerst dat je het importeert:
```Haskell
import qualified Data.Map as Map
```

Een kaart maken is eenvoudig. Laten we er een maken met enkele programmeertalen en hun paradigma’s:
```Haskell
let languages = Map.fromList [("Haskell", "Functioneel"), ("Python", "Imperatief"), ("Prolog", "Logisch")]
```

Hoe zit het met het krijgen van het paradigma van Haskell?
```Haskell
Map.lookup "Haskell" languages
-- output: Just "Functioneel"
```

Een nieuwe taal toevoegen is gemakkelijk:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systemen" languages
```

Wat als we alle talen willen opsommen? Gebruik `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- output: ["Haskell","Python","Prolog","Rust"]
```

Om de paradigma's te vermelden, gebruik je `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- output: ["Functioneel","Imperatief","Logisch","Systemen"]
```

Deze basishandelingen moeten voor de meeste toepassingen volstaan, maar er is nog veel meer te ontdekken in `Data.Map`!

## Diepe Duik

De `Data.Map` module in de standaardbibliotheek van Haskell is gebouwd op basis van gebalanceerde binaire bomen, specifiek AVL-bomen. Deze keuze zorgt ervoor dat de meeste bewerkingen op de kaart, zoals invoegen, verwijderen en opzoeken, kunnen worden gedaan in O(log n) tijd, waarbij n het aantal elementen in de kaart is. Het is een efficiënte keuze voor veel gebruiksscenario's, hoewel niet absoluut de snelste voor alle scenario's.

Er is ook een historische nuance: voordat `Data.Map` de standaard werd, gebruikten Haskell-programmeurs vaak lijsten van paren om associatieve arrays te simuleren. Echter, operaties op dergelijke structuren zijn O(n) voor opzoeking, waardoor `Data.Map` een aanzienlijke verbetering betekent in termen van prestaties.

Nu, ondanks de efficiëntie en bruikbaarheid van `Data.Map`, is het niet altijd het beste gereedschap voor elke klus. Voor taken die zeer prestatiegevoelig zijn, waarbij zelfs O(log n) opzoektijden te traag zijn, of waar sleutels altijd geheelwaardige waarden zijn, kunnen arrays of hashtabellen (via `Data.HashMap`) beter presteren met O(1) toegangstijden.

Het Haskell-ecosysteem biedt een verscheidenheid aan gegevensstructuren om aan verschillende behoeften te voldoen, en `Data.Map` is een uitstekende algemene keuze voor associatieve arrays, waarbij gebruiksgemak, flexibiliteit en prestaties in balans zijn.
