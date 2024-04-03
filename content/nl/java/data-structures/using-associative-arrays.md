---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:48.086970-07:00
description: "In Java laten associatieve arrays, of maps, je sleutel-waardeparen opslaan\
  \ voor effici\xEBnte gegevensopzoeking en -manipulatie. Programmeurs gebruiken ze\u2026"
lastmod: '2024-03-13T22:44:50.674186-06:00'
model: gpt-4-0125-preview
summary: "In Java laten associatieve arrays, of maps, je sleutel-waardeparen opslaan\
  \ voor effici\xEBnte gegevensopzoeking en -manipulatie."
title: Gebruik van associatieve arrays
weight: 15
---

## Wat & Waarom?

In Java laten associatieve arrays, of maps, je sleutel-waardeparen opslaan voor efficiënte gegevensopzoeking en -manipulatie. Programmeurs gebruiken ze voor taken zoals het tellen van het voorkomen van items of het toewijzen van gebruikers aan hun permissies omdat ze snelle toegang en updates bieden.

## Hoe te:

Java heeft geen ingebouwde associatieve arrays zoals sommige talen dat wel hebben, maar het biedt de `Map` interface en klassen zoals `HashMap` en `TreeMap` om die rol te vervullen. Hier is hoe je een `HashMap` gebruikt:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Een HashMap maken
        Map<String, Integer> leeftijdVanVrienden = new HashMap<>();
        
        // Elementen toevoegen
        leeftijdVanVrienden.put("Alice", 24);
        leeftijdVanVrienden.put("Bob", 30);
        leeftijdVanVrienden.put("Charlie", 28);

        // Elementen openen
        System.out.println("Alice's leeftijd: " + leeftijdVanVrienden.get("Alice"));
        
        // Omgaan met niet-bestaande sleutels
        System.out.println("Leeftijd van iemand die niet in de map staat: " + leeftijdVanVrienden.getOrDefault("Dan", -1));

        // Over elementen itereren
        for (Map.Entry<String, Integer> entry : leeftijdVanVrienden.entrySet()) {
            System.out.println(entry.getKey() + " is " + entry.getValue() + " jaar oud.");
        }
    }
}
```

Voorbeelduitvoer:

```
Alice's leeftijd: 24
Leeftijd van iemand die niet in de map staat: -1
Alice is 24 jaar oud.
Bob is 30 jaar oud.
Charlie is 28 jaar oud.
```

`HashMap` is slechts één implementatie. Als je unieke sleutels hebt en je hebt ze gesorteerd nodig, overweeg dan een `TreeMap`. Voor een map die de volgorde van invoeging behoudt, is `LinkedHashMap` je vriend.

## Diepgaande Duik

Maps in Java zijn onderdeel van de Collections Framework, geïntroduceerd in JDK 1.2, maar hebben in de loop der jaren aanzienlijke verbeteringen gezien, inclusief de introductie van de `forEach` methode in Java 8 voor eenvoudigere iteratie over entries. De keuze voor de mapimplementatie (`HashMap`, `LinkedHashMap`, `TreeMap`) moet worden bepaald door je specifieke behoeften op het gebied van ordening en prestaties. Bijvoorbeeld, `HashMap` biedt een O(1) tijdsprestatie voor de basisbewerkingen (get en put), ervan uitgaande dat de hashfunctie de elementen goed verspreidt over de emmers. Echter, als je sortering nodig hebt op basis van natuurlijke ordening of aangepaste vergelijkers, is `TreeMap` de juiste keuze, die O(log n) tijd biedt voor invoeging en opzoeking.

Voordat `Map` werd geïntroduceerd, werden associatieve arrays meestal geïmplementeerd met twee parallelle arrays (één voor sleutels, één voor waarden) of aangepaste datastructuren met minder efficiëntie. Huidige alternatieven voor `Map` en zijn implementaties kunnen third-party bibliotheken omvatten die gespecialiseerde maps aanbieden, zoals bidirectionele maps (BiMap in Google's Guava bibliotheek) voor gevallen waarin je efficiënt een sleutel op zijn waarde moet kunnen vinden. Echter, voor de meeste gebruiksscenario's in Java, zijn de standaardbibliotheek's maps robuust en flexibel genoeg om de taak aan te kunnen.
