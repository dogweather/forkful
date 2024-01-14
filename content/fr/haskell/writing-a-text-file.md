---
title:                "Haskell: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Les fichiers textes sont un outil essentiel pour les programmeurs Haskell, permettant de stocker des données et de les manipuler facilement dans un format lisible pour les humains. Ils sont également utiles pour stocker des configurations et des paramètres pour les programmes.

## Comment Faire

Pour créer un fichier texte en Haskell, il suffit d'utiliser la fonction intégrée `writeFile` en spécifiant le nom du fichier et le contenu que vous souhaitez y mettre. Par exemple:

```Haskell
-- Création d'un nouveau fichier texte nommé "nombres.txt"
writeFile "nombres.txt" "1\n2\n3\n4\n5\n"
```

Une fois le fichier créé, vous pouvez le lire en utilisant la fonction `readFile` et en spécifiant le nom du fichier. Par exemple:

```Haskell
-- Lecture du contenu du fichier "nombres.txt"
nombres <- readFile "nombres.txt"
```

Pour écrire et lire des fichiers texte plus complexes, vous pouvez utiliser des fonctions de traitement de chaînes de caractères telles que `split` et `join` pour diviser et combiner les données dans le format souhaité.

## Plongée Profonde

Lors de l'écriture de fichiers texte en Haskell, il est important de prêter attention aux encodages de caractères utilisés. Haskell utilise par défaut l'encodage UTF-8, mais il est possible de spécifier un autre encodage en utilisant la fonction `writeFile` avec le paramètre optionnel `encoding`.

De plus, il est important de noter que les fichiers texte en Haskell sont considérés comme immuables, ce qui signifie qu'ils ne peuvent pas être modifiés une fois créés. Cela garantit la stabilité et la sécurité des données stockées dans ces fichiers.

## Voir Aussi

- [Documentation sur la fonction `writeFile`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:writeFile)
- [Documentation sur la fonction `readFile`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:readFile)
- [Documentation sur les fonctions de traitement de chaînes de caractères](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:split)
- [Documentation sur les encodages de caractères en Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:openFile)