---
title:                "Verwendung von assoziativen Arrays"
aliases: - /de/java/using-associative-arrays.md
date:                  2024-01-30T19:11:41.047999-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

In Java ermöglichen assoziative Arrays, oder Maps, das Speichern von Schlüssel-Wert-Paaren für eine effiziente Datensuche und -manipulation. Programmierer nutzen sie für Aufgaben wie das Zählen von Vorkommen von Elementen oder das Zuordnen von Benutzern zu ihren Berechtigungen, da sie schnellen Zugriff und Aktualisierungen bieten.

## Wie geht das:

Java verfügt nicht über eingebaute assoziative Arrays wie einige andere Sprachen, stellt aber die `Map` Schnittstelle und Klassen wie `HashMap` und `TreeMap` zur Verfügung, um diese Rolle zu erfüllen. So verwenden Sie eine `HashMap`:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // Erstellen einer HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // Elemente hinzufügen
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // Elemente zugreifen
        System.out.println("Alter von Alice: " + ageOfFriends.get("Alice"));
        
        // Umgang mit nicht vorhandenen Schlüsseln
        System.out.println("Alter von jemandem, der nicht in der Map ist: " + ageOfFriends.getOrDefault("Dan", -1));

        // Iterieren über Elemente
        for (Map.Entry<String, Integer> Eintrag : ageOfFriends.entrySet()) {
            System.out.println(Eintrag.getKey() + " ist " + Eintrag.getValue() + " Jahre alt.");
        }
    }
}
```

Beispielausgabe:

```
Alter von Alice: 24
Alter von jemandem, der nicht in der Map ist: -1
Alice ist 24 Jahre alt.
Bob ist 30 Jahre alt.
Charlie ist 28 Jahre alt.
```

`HashMap` ist nur eine Implementierung. Wenn Ihre Schlüssel einzigartig sind und Sie diese sortiert benötigen, sollten Sie `TreeMap` in Betracht ziehen. Für eine Map, die die Einfügereihenfolge beibehält, ist `LinkedHashMap` Ihr Freund.

## Tiefer eintauchen

Maps in Java sind Teil des Collections Frameworks, eingeführt in JDK 1.2, haben aber über die Jahre bedeutende Verbesserungen erfahren, einschließlich der Einführung der `forEach` Methode in Java 8 für eine einfachere Iteration über Einträge. Die Wahl der Map-Implementierung (`HashMap`, `LinkedHashMap`, `TreeMap`) sollte von Ihren spezifischen Bedürfnissen in Bezug auf Ordnung und Leistung bestimmt werden. Zum Beispiel bietet `HashMap` O(1) Zeitperformance für die grundlegenden Operationen (get und put), vorausgesetzt, die Hashfunktion verteilt die Elemente ordnungsgemäß unter den Buckets. Allerdings, wenn Sie eine Sortierung basierend auf natürlicher Ordnung oder benutzerdefinierten Vergleichsfunktionen benötigen, ist `TreeMap` die richtige Wahl und bietet O(log n) Zeit für Einfügung und Suche.

Bevor `Map` eingeführt wurde, wurden assoziative Arrays üblicherweise mit zwei parallelen Arrays (eines für Schlüssel, eines für Werte) oder benutzerdefinierten Datenstrukturen mit geringerer Effizienz implementiert. Aktuelle Alternativen zu `Map` und seinen Implementierungen könnten Drittanbieter-Bibliotheken beinhalten, die spezialisierte Maps anbieten, wie z. B. bidirektionale Maps (BiMap in Googles Guava-Bibliothek) für Fälle, in denen Sie effizient einen Schlüssel nach seinem Wert finden müssen. Jedoch sind für die meisten Anwendungsfälle in Java die Maps der Standardbibliothek robust und flexibel genug, um die Aufgabe zu bewältigen.
