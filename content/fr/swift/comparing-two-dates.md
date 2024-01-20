---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## C'est quoi & Pourquoi?

Comparer deux dates, c'est simplement déterminer si une date est antérieure, postérieure ou équivalente à une autre en termes de notre flux temporel. Pouquoi cela? Les programmeurs font cela pour contrôler des scénarios cruciaux comme la sortie d'un produit, le respect d'un délai, et les tâches périodiques, entre autres.

## Comment faire:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

let date1 = dateFormatter.date(from: "2022/01/01 00:01")!
let date2 = dateFormatter.date(from: "2021/12/31 23:59")!

if date1.compare(date2) == .orderedDescending {
    print("date1 est postérieure à date2")
} else if date1.compare(date2) == .orderedAscending {
    print("date1 est antérieure à date2")
} else {
     print("Les deux dates sont identiques")
}
```

```Exemple de sortie:
date1 est postérieure à date2
```
## Plongée en profondeur

Historiquement, les langages de programmation ont toujours eu une variété de méthodes pour comparer deux dates, allant de bibliothèques tierces aux méthodes intégrées. Swift a simplifié cette tâche en intégrant une méthode de comparaison de date directement dans la bibliothèque standard.

En ce qui concerne les alternatives, vous pouvez également utiliser les opérateurs `<`, `>` et `==` pour une syntaxe plus succincte.

```Swift
if date1 > date2 {
    print("date1 est postérieure à date2")
}
```

Quant aux détails de l'implémentation, la méthode `compare(_:)` renvoie une valeur de type `ComparisonResult`, qui est une énumération avec trois cas possibles: `.orderedAscending`, `.orderedSame` et `.orderedDescending`. L'énumération facilite la détermination de la relation de comparaison entre deux dates.

## Voir aussi

[Documentation Apple sur Date](https://developer.apple.com/documentation/foundation/date)<br>
[Tutoriel sur les opérations de date et heure en Swift](https://www.hackingwithswift.com/read/1/3/variables-and-type-annotations)<br>
[Guide d’Apple sur les dates et les heures](https://developer.apple.com/documentation/foundation/time_and_date)