---
title:                "Capitaliser une chaîne de caractères"
html_title:           "Gleam: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé pourquoi certaines fonctions de traitement de chaînes de caractères en programmation sont en majuscules et d'autres en minuscules ? Ou peut-être avez-vous simplement besoin de capitaliser une chaîne de caractères pour des raisons esthétiques ou dans le cadre d'un projet spécifique. Dans cet article, nous allons vous montrer comment capitaliser une chaîne de caractères en utilisant le langage de programmation Gleam, afin que vous puissiez ajouter cette compétence utile à votre boîte à outils de développement.

## Comment faire

Pour capitaliser une chaîne de caractères en utilisant Gleam, nous allons utiliser une fonction intégrée appelée `String.capitalize`. Voici un exemple de code pour capitaliser une chaîne de caractères simple :

```Gleam
let string = "bonjour"
let capitalized_string = String.capitalize(string)
```

Et voici le résultat lorsque nous imprimons la nouvelle chaîne de caractères :

```Gleam
"Bonjour"
```

Comme vous pouvez le voir, la fonction `String.capitalize` prend simplement une chaîne de caractères en paramètre et renvoie une nouvelle chaîne de caractères avec la première lettre en majuscule.

Vous pouvez également utiliser cette fonction sur des chaînes de caractères plus longues, comme dans cet exemple :

```Gleam
let phrase = "voilà une phrase avec des mots en majuscule"
let capitalized_phrase = String.capitalize(phrase)
```

Et voici le résultat :

```Gleam
"Voilà une phrase avec des mots en majuscule"
```

## Plongée en profondeur

Si vous souhaitez capitaliser des chaînes de caractères avec des règles plus spécifiques, Gleam offre également la fonction `String.capitalize_words`. Cette fonction capitalisera chaque mot individuellement, ce qui peut être utile pour des titres ou des noms propres.

Voici un exemple de code utilisant `String.capitalize_words` :

```Gleam
let phrase = "le développement avec gleam est amusant"
let capitalized_phrase = String.capitalize_words(phrase)
```

Et le résultat :

```Gleam
"Le Développement Avec Gleam Est Amusant"
```

Comme vous pouvez le voir, chaque mot a été capitalisé individuellement. Vous pouvez également utiliser cette fonction pour spécifier des exceptions, par exemple si vous voulez que certains mots restent en minuscules.

## Voir aussi

- [Documentation officielle de Gleam] (https://gleam.run/docs/)
- [Autres fonctions de traitement de chaînes en Gleam] (https://help.disqus.com/)

N'oubliez pas de consulter la documentation officielle de Gleam pour en savoir plus sur les fonctions de traitement de chaînes et comment les utiliser dans vos projets. Et si vous avez des questions ou des commentaires, n'hésitez pas à les partager sur la communauté Gleam !