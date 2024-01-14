---
title:                "Gleam: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous en avez assez de devoir remplacer manuellement des mots ou phrases dans votre code ? Ou bien vous travaillez sur un projet avec plusieurs collaborateurs et il est difficile de garder une cohérence dans le choix des termes utilisés ? Heureusement, grâce à Gleam, il existe une solution simple et efficace pour effectuer des recherches et remplacements de texte : la fonction `replace`. Dans cet article, nous allons vous expliquer comment l'utiliser.

## Comment faire

La fonction `replace` prend trois arguments : une chaîne de caractères à modifier, la chaîne de caractères à rechercher et la chaîne de caractères de remplacement. Voici un exemple de code avec une chaîne de caractères `message` que nous voulons modifier et une chaîne de caractères `old_word` que nous souhaitons remplacer par une chaîne de caractères `new_word` :

```Gleam
let message = "J'utilise Gleam pour écrire mon code."
let old_word = "Gleam"
let new_word = "un langage"
let updated_message = replace(message, old_word, new_word)
```

Dans cet exemple, nous avons utilisé la fonction `replace` pour remplacer le mot "Gleam" par "un langage" dans notre chaîne de caractères `message`. Si nous affichons la valeur de `updated_message`, nous obtenons le résultat suivant : "J'utilise un langage pour écrire mon code." La fonction `replace` a automatiquement trouvé toutes les occurrences de "Gleam" dans la chaîne de caractères `message` et les a remplacées par "un langage".

## Plongée en profondeur

Il est possible de spécifier une quatrième option à la fonction `replace` pour indiquer si l'on souhaite effectuer la recherche de manière sensible à la casse ou non. Par défaut, la recherche est sensible à la casse, ce qui signifie que les lettres majuscules et minuscules seront prises en compte lors de la recherche et du remplacement. Par exemple, dans notre exemple précédent, si nous avions cherché à remplacer le mot "gleam" plutôt que "Gleam", la fonction `replace` n'aurait pas trouvé de correspondance et n'aurait rien remplacé.

Si nous voulons que la recherche soit insensible à la casse, il suffit de spécifier `false` comme quatrième argument :

```Gleam
let message = "J'utilise Gleam pour écrire mon code."
let old_word = "gleam"
let new_word = "un langage"
let updated_message = replace(message, old_word, new_word, false)
```

Dans ce cas, la valeur de `updated_message` serait toujours "J'utilise un langage pour écrire mon code." quel que soit le cas des lettres dans les différentes occurrences de "Gleam".

## Voir aussi

Pour en savoir plus sur la fonction `replace` et d'autres fonctionnalités de Gleam, vous pouvez consulter les liens suivants :

- Documentation officielle de Gleam : https://gleam.run/documentation/
- Tutoriels sur Gleam : https://gleam.run/tutorials/

À vous de jouer ! Améliorez votre productivité grâce à la fonction `replace` et n'hésitez pas à explorer toutes les autres possibilités offertes par Gleam pour faciliter votre travail de développement.