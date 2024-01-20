---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'extraction de sous-chaînes est un aspect fondamental de la manipulation des chaînes de caractères. Les programmeurs l'utilisent pour trouver et utiliser des morceaux spécifiques de texte à l'intérieur d'une chaîne de caractères plus conséquente.

## Comment faire:
En Clojure, nous utilisons principalement la fonction `subs`. Voici un exemple de base:

```Clojure
(subs "Bonjour le monde!" 8 11)
```

La sortie sera:

```Clojure
"le"
```

Dans l'exemple ci-dessus, nous avons extrait la sous-chaîne commençant à l'index 8 et se terminant à l'index 11 de la chaîne "Bonjour le monde!"

## Plongée en profondeur
Historiquement, `subs` est à la base de la manipulation de chaînes en Clojure. Il n'est pas aussi riche en fonctionnalités que certaines alternatives dans d'autres langages, mais sa simplicité en fait un choix populaire. 

De nombreuses alternatives existent pour extraire des sous-chaînes, y compris l'utilisation de `split` pour diviser sur un délimiteur et extraire un index spécifique. 

Par exemple:
```Clojure
(nth (clojure.string/split "Bonjour le monde!" #" ") 1)
```

Il est important de noter que l'indexage en Clojure commence à 0, donc 1 renvoie le deuxième mot, "le".

Les mécanismes internes de `subs` sont plutôt directs. La source de Clojure montre que `subs` convertit la chaîne d'entrée en un type Java String, puis utilise la méthode `substring()` de Java pour effectuer l'opération réelle.

## Voir Aussi
Pour plus de détails sur `subs` et d'autres fonctions liées à la chaîne, consultez la documentation officielle Clojure [ici](https://clojuredocs.org/clojure.core/subs). 

Pour des exemples de manipulation de chaînes plus complexes, consultez [ce post](https://clojure-cookbook.com/posts-output/2013-04-04-manipulating-strings-with-clojure) sur le Clojure Cookbook.