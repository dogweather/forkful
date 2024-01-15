---
title:                "Extraction de sous-chaînes"
html_title:           "Elm: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Tu as déjà eu besoin de extraire un morceau de texte à partir d'une chaîne de caractères en Elm ? Peut-être que tu as un formulaire où tu dois valider une adresse e-mail à partir de laquelle tu veux extraire le domaine. Ou peut-être que tu veux extraire le nom d'utilisateur d'une URL pour afficher des informations personnalisées sur une page. Dans ces cas-là, savoir comment extraire des sous-chaines en Elm peut être très utile.

# Comment faire

C'est très simple de extraire des substrings en Elm. Tout d'abord, tu dois utiliser la fonction `String.dropLeft` ou `String.dropRight` pour indiquer à Elm combien de caractères tu veux enlever du début ou de la fin de ta chaîne de caractères. Ensuite, utilise la fonction `String.slice` pour extraire le morceau de ta chaîne de caractères en utilisant l'index du début et de la fin que tu veux garder.

```Elm
input = "john.smith@gmail.com" 
domain = String.slice 11 -4 input
-- output: "gmail"
```

Tu peux également utiliser l'indexation par caractère pour extraire une sous-chaîne en utilisant la fonction `String.map`. Par exemple, si tu veux extraire le prénom `john` à partir de `john.smith@gmail.com`, tu peux utiliser `String.map` pour trouver l'index du premier point et extraire les caractères avant cet index.

```Elm
input = "john.smith@gmail.com"
firstName = input |> String.map toSlug |> String.dropRight 10
-- output: "john"
```

# Plongeon en profondeur

Il y a quelques choses à garder à l'esprit lors de l'extraction de substrings en Elm. Tout d'abord, la fonction `String.dropLeft` renverra une chaîne de caractères vide si tu essaies de supprimer plus de caractères qu'il n'y en a dans ta chaîne de caractères. Cela peut causer des erreurs si tu ne vérifies pas cela dans ton code. Deuxièmement, la fonction `String.dropRight` renverra une erreur si tu utilises un index négatif. Donc, assure-toi de toujours utiliser un index positif avec cette fonction.

# Voir aussi

- Documentation officielle d'Elm sur la manipulation de chaînes de caractères : https://guide.elm-lang.org/strings/
- Un tutoriel sur la manipulation de chaînes de caractères en Elm : https://dev.to/benjaminadk/elm-strings-1ckc
- Exemples pratiques d'utilisation de fonctions pour extraire des sous-chaînes en Elm : https://medium.com/@svalaskevicius/elm-string-operations-compared-26f71961c9d4