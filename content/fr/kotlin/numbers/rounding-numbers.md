---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:45:27.058769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Arrondir des nombres signifie les ajuster au nombre entier le plus proche ou à un degré de précision spécifié. Les programmeurs le font pour améliorer la lisibilité, réduire les exigences de stockage, ou parce que la valeur exacte n'est pas critique pour les calculs subséquents.

## Comment faire :

En Kotlin, l'arrondi peut être réalisé en utilisant plusieurs fonctions telles que `roundToInt()`, `roundToDouble()`, et en utilisant `BigDecimal` pour plus de contrôle :

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Affiche : 3

    val number2 = 3.5
    println(number2.roundToInt()) // Affiche : 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Affiche : 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Affiche : 123.5
}
```

## Plongée Profonde

Historiquement, arrondir les nombres a été un concept fondamental en mathématiques et en informatique, conçu pour gérer les limitations de précision numérique. Dans les débuts de l'informatique, l'arrondi était crucial en raison du coût élevé de la mémoire.

En Kotlin, l'arrondi est basé sur les bibliothèques Java standard. Les options pour l'arrondi incluent `Math.round()`, qui arrondit au nombre entier le plus proche, et `BigDecimal` pour un arrondi personnalisable, où vous pouvez spécifier une échelle et un `RoundingMode`.

Chaque `RoundingMode` a des politiques différentes pour gérer les égalités (quand le chiffre est exactement au milieu des options d'arrondi). Par exemple, `RoundingMode.HALF_UP` arrondit vers le voisin le plus proche, à moins que les deux voisins ne soient à égale distance, auquel cas il arrondit à l'entier supérieur.

## Voir Aussi

- Documentation Kotlin sur [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Documentation Java d'Oracle pour [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754) [Norme IEEE 754](https://ieeexplore.ieee.org/document/4610935)
