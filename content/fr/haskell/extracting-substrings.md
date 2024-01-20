---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

**## Qu'est-ce Que C'est & Pourquoi?**

L'extraction de sous-chaînes, c'est l'action de prendre une petite partie d'une chaîne de caractères plus large. Les développeurs font cela pour manipuler et analyser des portions spécifiques de données textuelles.

**## Comment Faire:**

En Haskell, nous utilisons la fonction `take` pour extraire des sous-chaînes. En voici un exemple : 

```Haskell
let chaine = "Salut tout le monde"
print(take 5 chaine)
```

Cela affichera `"Salut"` dans la console.

On utilise aussi `drop` pour supprimer le début d'une chaîne:

```Haskell
print(drop 6 chaine)
```

Cela affichera `"tout le monde"`.

**## Aperçu Approfondi:**

Historiquement parlant, l'extraction de sous-chaînes a toujours été un élément clé de la manipulation de texte. En Haskell, ces fonctions ont été inspirées par d'autres langages de programmation fonctionnels.

Il existe des alternatives à `take` et `drop`. Par exemple, `splitAt` qui divise une chaîne en un tuple à un index donné.

La mise en œuvre de ces fonctions est assez linéaire. `take` et `drop` parcourent simplement la liste de gauche à droite.

**## Voir Aussi:**

- Pour plus d'infos sur `take` et `drop` en Haskell, consultez la documentation officielle ici: http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
- Pour une discussion plus générale sur la manipulation de chaînes en Haskell, visitez: http://learnyouahaskell.com/input-and-output.