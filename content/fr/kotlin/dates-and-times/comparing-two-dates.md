---
date: 2024-01-20 17:33:21.365582-07:00
description: "Comment faire : Historiquement en Java, les dates \xE9taient g\xE9r\xE9\
  es par `java.util.Date`, mais cette classe avait des probl\xE8mes : difficile \xE0\
  \ utiliser et pas\u2026"
lastmod: '2024-04-05T22:38:58.297767-06:00'
model: gpt-4-1106-preview
summary: "Historiquement en Java, les dates \xE9taient g\xE9r\xE9es par `java.util.Date`,\
  \ mais cette classe avait des probl\xE8mes ."
title: Comparer deux dates
weight: 27
---

## Comment faire :
```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 1)
    val date2 = LocalDate.now()
    
    println(date1.compareTo(date2)) // Avant aujourd'hui : -1, après : 1, même jour : 0
    println(date1.isBefore(date2))  // true si date1 est avant date2
    println(date1.isAfter(date2))   // true si date1 est après date2
    println(date1.isEqual(date2))   // true si les deux dates sont les mêmes
}

// Exemple de sortie:
// -1
// true
// false
// false
```

## Plongée profonde :
Historiquement en Java, les dates étaient gérées par `java.util.Date`, mais cette classe avait des problèmes : difficile à utiliser et pas thread-safe. Depuis Java 8, `java.time.LocalDate` est le choix préféré pour les dates sans horaires. Comparer deux `LocalDate` se fait facilement grâce à `compareTo`, `isBefore`, `isAfter`, et `isEqual`. Ces méthodes suivent la norme ISO-8601 et prennent en compte les années bissextiles. Autrement, des bibliothèques comme Joda-Time offraient ces fonctionnalités, mais elles sont maintenant moins utilisées grâce à `java.time`.

## Voir également :
- [Documentation officielle de LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Guide sur `java.time` de Baeldung](https://www.baeldung.com/java-8-date-time-intro)
- [Différences entre `java.util.Date` et `java.time.LocalDate`](https://www.baeldung.com/java-date-to-localdate-and-localdatetime)
