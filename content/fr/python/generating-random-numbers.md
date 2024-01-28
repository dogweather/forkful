---
title:                "Génération de nombres aléatoires"
date:                  2024-01-27T20:34:48.799899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La génération de nombres aléatoires consiste à créer des nombres qui ne peuvent pas être prédits de manière raisonnable autrement que par hasard, ce qui est essentiel pour le développement de simulations, de jeux et d'algorithmes de sécurité. Les programmeurs font cela pour introduire de l'imprévisibilité ou pour simuler des phénomènes du monde réel dans leurs applications.

## Comment faire :

Python fournit le module `random` qui aide à générer des nombres aléatoires pour divers usages. Voici comment commencer :

1. **Importer le module**
    ```Python
    import random
    ```

2. **Générer un entier aléatoire**
    Entre n'importe quels deux nombres.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Exemple de sortie : `7`

3. **Générer un nombre à virgule**
    Entre 0 et 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Exemple de sortie : `0.436432634653`

    Si vous avez besoin d'un flottant dans une plage différente, multipliez :
    ```Python
    random_float_range = random.random() * 5  # de 0 à 5
    print(random_float_range)
    ```
    Exemple de sortie : `3.182093745`

4. **Choisir un élément aléatoire d'une liste**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Exemple de sortie : `Hola`

5. **Mélanger une liste**
    Parfait pour les jeux de cartes ou toute application ayant besoin de randomiser l'ordre.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Exemple de sortie : `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Exploration en profondeur

Le module `random` de Python utilise un générateur de nombres pseudo-aléatoires (PRNG), spécifiquement l'algorithme Mersenne Twister, qui est bon pour les applications à usage général mais pas adapté à des fins cryptographiques en raison de sa prévisibilité si suffisamment de sorties sont observées. Le module `secrets`, introduit dans Python 3.6, offre une meilleure alternative pour générer des nombres aléatoires cryptographiquement forts, particulièrement utile dans les applications sensibles à la sécurité. Par exemple, générer un jeton aléatoire sécurisé pour un lien de réinitialisation de mot de passe :

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historiquement, générer des nombres aléatoires qui sont véritablement aléatoires a été un défi en informatique, avec des méthodes précoces reposant sur des phénomènes physiques ou des graines entrées manuellement. Le développement et l'adoption d'algorithmes comme Mersenne Twister (utilisé par défaut dans le module `random` de Python jusqu'à au moins ma dernière mise à jour des connaissances en 2023) ont marqué des progrès significatifs. Toutefois, la recherche continue de algorithmes plus sécurisés et efficaces a mené à l'inclusion du module `secrets` pour les tâches liées à la cryptographie. Cette évolution reflète l'importance croissante de la sécurité dans le développement logiciel et le besoin de plus robustesse en matière d'aléatoire dans des applications allant du chiffrement à la génération de jetons sécurisés.
