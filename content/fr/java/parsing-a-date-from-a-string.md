---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:36:49.049467-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Convertir une chaîne de caractères en date nous permet de manipuler cette dernière dans nos programmes — utile pour la comparaison, le stockage, ou la manipulation temporelle. Les devs font ça pour clarifier et utiliser les données de time-stamping ou d’entrée utilisateur.

## Comment faire :
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "14/03/2023"; // Format jour/mois/année
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        LocalDate date = LocalDate.parse(dateString, formatter);
        
        System.out.println("Date parse: " + date); // Affiche la date traitée
    }
}
```
Sortie:
```
Date parse: 2023-03-14
```

## Plongée Profonde
À l'origine, Java utilisait `SimpleDateFormat` de `java.text` qui était moins sûr et immuable. `DateTimeFormatter` fait partie de java.time, introduit dans Java 8, plus robuste et thread-safe. Alternativement, des bibliothèques tierces comme Joda-Time existaient avant Java 8, mais java.time est désormais le standard de facto. La clé de l'implémentation est dans les motifs de formatage: comprendre des lettres comme 'd' pour le jour, 'M' pour le mois. Attention au détail avec majuscules et minuscules, car elles déterminent l'exactitude du composant de la date (par exemple, 'MM' est le mois en deux chiffres et 'MMM' pour le format abrégé du mois).

## Voir aussi
- Documentation officielle de l'API java.time: [Java SE Time Package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Histoire de Joda-Time et sa relation avec java.time : [Joda-Time GitHub](https://www.joda.org/joda-time/)
- Formatage et analyse de dates avec Java : [DateTimeFormatter JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
