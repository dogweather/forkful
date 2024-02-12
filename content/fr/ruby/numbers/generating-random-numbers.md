---
title:                "Génération de nombres aléatoires"
aliases:
- /fr/ruby/generating-random-numbers/
date:                  2024-01-27T20:34:51.392934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en Ruby implique la création de nombres qui ne peuvent pas être prédits logiquement, essentiel pour des scénarios tels que les simulations, la cryptographie, et les jeux. Les programmeurs utilisent l'aléatoire pour ajouter de l'imprévisibilité ou imiter les variabilités de la vie réelle dans leurs applications.

## Comment faire :

Ruby fournit plusieurs méthodes pour générer des nombres aléatoires, principalement via la classe `Random`.

### Nombre Aléatoire Basique

Pour générer un nombre aléatoire basique :

```Ruby
puts rand(10) # Génère un nombre aléatoire entre 0 et 9
```

### Nombre Aléatoire Dans une Plage

Pour un nombre aléatoire dans une plage spécifique :

```Ruby
puts rand(1..10) # Génère un nombre aléatoire entre 1 et 10
```

### Utiliser la Classe Random

Pour créer une séquence répétable de nombres aléatoires, vous pouvez utiliser la classe `Random` avec une graine.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Génère un nombre "aléatoire" prévisible
```

### Générer un Élément Aléatoire d'un Tableau

Sélectionnez un élément aléatoire d'un tableau :

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Sélectionne aléatoirement un élément du tableau
```

### Exemple de Sortie :

Chaque extrait de code ci-dessus, une fois exécuté, produira des sorties différentes en raison de leur nature aléatoire. Par exemple, `rand(10)` pourrait produire `7`, tandis que `colors.sample` pourrait produire `"green"`.

## Plongée Approfondie

Le concept de génération de nombres aléatoires en informatique est paradoxal car les ordinateurs suivent des instructions déterministes. Les méthodes anciennes dépendaient fortement de l'entrée externe pour atteindre l'imprévisibilité. La randomisation en Ruby est construite sur l'algorithme Mersenne Twister, un générateur de nombres pseudo-aléatoires connu pour sa vaste période et sa distribution uniforme, le rendant hautement adapté aux applications nécessitant une haute qualité d'aléatoire.

Bien que les méthodes intégrées de Ruby répondent bien à la plupart des besoins, elles pourraient ne pas suffire pour tous les objectifs cryptographiques, car la prévisibilité des nombres pseudo-aléatoires peut être une vulnérabilité. Pour la sécurité cryptographique, les développeurs Ruby pourraient explorer des bibliothèques comme `OpenSSL::Random`, qui sont conçues pour produire des nombres aléatoires cryptographiquement sécurisés, assurant une imprévisibilité plus élevée pour les applications sensibles.
