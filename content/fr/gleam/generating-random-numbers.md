---
title:                "Gleam: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Se lancer dans la génération de nombres aléatoires peut sembler une tâche futile en apparence, mais en réalité, c'est une compétence très utile pour tout programmeur. Que ce soit pour générer des données de test, simuler des scénarios aléatoires ou simplement ajouter un élément de surprise dans votre code, la génération de nombres aléatoires est un outil nécessaire pour de nombreuses tâches de programmation.

# Comment faire

Pour générer des nombres aléatoires en Gleam, vous pouvez utiliser la fonction `random.int` qui prend comme arguments le nombre minimum et maximum que vous souhaitez inclure dans la plage.

```
Gleam>
import gleam/random

random.int(1,100)

// Exemple de sortie: 57
```

Vous pouvez également générer des nombres réels aléatoires en utilisant la fonction `random.float` et spécifier un nombre de décimales à inclure.

```
Gleam>
import gleam/random

random.float(0.0,1.0, 2)

// Exemple de sortie: 0.89
```

Vous pouvez également utiliser la fonction `random.bool` pour générer des valeurs booléennes aléatoires, utiles pour simuler des conditions aléatoires dans vos tests.

```
Gleam>
import gleam/random

random.bool()

// Exemple de sortie: true
```

# Plongée en profondeur

Maintenant que vous avez une idée de comment générer des nombres aléatoires en Gleam, il est important de comprendre comment ces nombres sont générés. En interne, Gleam utilise l'algorithme Mersenne Twister pour générer des nombres pseudo-aléatoires. Cet algorithme est très efficace et produit des résultats de haute qualité.

Il est également important de noter que les nombres aléatoires générés par ordinateur ne sont jamais réellement aléatoires. Ils sont basés sur une séquence de valeurs prédictibles, mais en apparence, ils semblent aléatoires. Cela peut avoir un impact sur la sécurité si vous utilisez des nombres générés aléatoirement pour des opérations cryptographiques.

# Voir aussi

- [Documentation Gleam pour la génération de nombres aléatoires](https://gleam.run/modules/gleam/random/latest/)
- [Article sur l'algorithme Mersenne Twister](https://prng.di.unimi.it/)
- [Exemples de génération de nombres aléatoires en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/random_numbers.gleam)