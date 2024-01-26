---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:55.483216-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why (Quoi & Pourquoi) ?
Générer des nombres aléatoires, c'est faire sortir des numéros au pif, sans ordre ni raison. Les devs s'en servent pour tout et rien : des jeux aux simulations, en passant par les tests de sécurité.

## How to (Comment faire) :
Swift rend le truc facile avec saStandard Library. Voici les bases pour tirer des numéros au sort :

```Swift
import Foundation // Pas toujours nécessaire, mais parfois utile pour plus de fonctions

// Un nombre aléatoire entre 0 et 1
let randomFraction = Double.random(in: 0...1)

// Un entier aléatoire entre 1 et 100
let randomInt = Int.random(in: 1...100)

// Tirage aléatoire pour un élément dans un tableau
let items = ["pomme", "banane", "cerise"]
if let randomItem = items.randomElement() {
    print(randomItem)
}

// Mélanger un tableau de manière aléatoire
var numbers = [1, 2, 3, 4, 5]
numbers.shuffle()
```

Avec ça, tu peux avoir :

```
cerise
[3, 5, 1, 4, 2]
```

Bien sûr, le résultat va changer à chaque fois. C'est tout l'intérêt.

## Deep Dive (Plongée Profonde) :
Avant Swift, c'était la galère avec `arc4random()` et ses potes, pas toujours évident à utiliser correctement. Maintenant, avec Swift, on a une API simple et sûre qui s'appuie sur des générateurs de nombres aléatoires (GNA) de haute qualité, sans risque d'erreurs.

Alternativement, si t'as besoin de plus de contrôle, Swift te propose aussi des GNA custom via le protocol `RandomNumberGenerator`. Tu peux même construire ton propre GNA si t'es du genre minutieux. Mais pour la majorité des mortels, les fonctions de base suffisent.

En matière de détails d'implémentation, sache que les fonctions aléatoires de Swift utilisent généralement un GNA appelé "arc4random" sous iOS, qui est sécurisé et suffisamment imprévisible pour l'usage commun. Bien sûr, pour les applications critiques (comme le cryptage), il faudrait aller voir du côté de générateurs spécialisés.

## See Also (Voir Aussi) :
Pour creuser davantage, je recommande :

- Une discussion sympa sur les GNA : [Random number generator](https://en.wikipedia.org/wiki/Random_number_generator)
- Pour les curieux des entrailles de la lib standard de Swift : [Swift Standard Library Source](https://github.com/apple/swift/tree/main/stdlib/public/core)

N'hésite pas à expérimenter et à plonger dans le code pour bien comprendre ce qui se passe sous le capot. Bon code !
