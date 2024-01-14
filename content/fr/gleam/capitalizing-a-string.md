---
title:                "Gleam: Majuscule d'une chaîne de caractères"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment capitaliser une chaîne de caractères en Gleam ? Eh bien, dans ce billet de blog, nous allons vous montrer comment le faire facilement et efficacement.

## Comment faire

Tout d'abord, créons une fonction qui va prendre une chaîne de caractères en argument et la capitaliser. Nous allons l'appeler "capitalize_string" :

```Gleam
pub fn capitalize_string(string: String) {
    let capitalized = string.to_uppercase();
    io.println(capitalized);
}
```

Dans cet exemple, nous utilisons la fonction prédéfinie `to_uppercase` pour transformer la chaîne de caractères en majuscule, puis nous utilisons la fonction `println` pour l'afficher à l'écran.

Maintenant, utilisons cette fonction dans notre programme :

```Gleam
let string = "salut";
capitalize_string(string);
```

Et voici le résultat :

```
"SALUT"
```

Comme vous pouvez le voir, notre fonction a bien capitalisé la chaîne de caractères "salut". Vous pouvez également utiliser cette fonction pour capitaliser des chaînes de caractères plus longues ou pour capitaliser les lettres d'un mot spécifique dans une phrase.

## Plongée en profondeur

Maintenant que nous avons vu comment capitaliser une chaîne de caractères, examinons de plus près ce qui se passe dans notre fonction.

Tout d'abord, nous utilisons la fonction `pub` avant notre fonction. Cela signifie qu'elle sera accessible depuis d'autres fichiers et modules. Ensuite, dans les parenthèses, nous spécifions que notre fonction aura un argument `string` de type `String`.

Ensuite, nous utilisons la fonction `to_uppercase` pour transformer la chaîne de caractères en majuscule. Cette fonction fait partie du module `String`, qui est importé automatiquement dans tous les fichiers Gleam.

Enfin, nous utilisons la fonction `println` pour afficher notre chaîne de caractères capitalisée à l'écran. Cette fonction fait partie du module `io`, qui doit être importé explicitement dans notre fichier pour être utilisé.

Vous pouvez également personnaliser cette fonction en lui ajoutant plus d'arguments, comme un caractère spécifique à utiliser comme séparateur entre chaque mot capitalisé.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/)
- [Chaînes de caractères en Gleam](https://gleam.run/book/tour/types/string.html)
- [Fonctions en Gleam](https://gleam.run/book/tour/functions.html)