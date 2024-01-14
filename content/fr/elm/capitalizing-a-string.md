---
title:    "Elm: Capitaliser une chaîne de caractères"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi 

Il y a de nombreuses raisons pour lesquelles vous pourriez vouloir utiliser la fonction `String.capitalize` en Elm. Peut-être que vous avez besoin de mettre en majuscule le premier caractère d'une chaîne de caractères pour des raisons de présentation ou peut-être avez-vous besoin de capitaliser un nom dans un formulaire. Quelle que soit la raison, la fonction `String.capitalize` est un outil utile à avoir dans votre boîte à outils de programmation Elm.

## Comment faire 

La fonction `String.capitalize` est extrêmement simple à utiliser. Il suffit de lui passer une chaîne de caractères en tant que paramètre et elle renverra cette même chaîne avec le premier caractère en majuscule. Par exemple :

```Elm 
String.capitalize "elm" 
```

Résultat : "Elm"

Il est important de noter que la fonction `String.capitalize` ne modifie pas la chaîne de caractères d'origine, elle renvoie simplement une nouvelle chaîne avec le premier caractère en majuscule. Cela peut être utile si vous avez besoin de garder la chaîne d'origine pour une utilisation ultérieure.

## Plongée en profondeur 

La fonction `String.capitalize` utilise la méthode `String.toUpper` pour capitaliser le premier caractère de la chaîne. Cela signifie qu'elle peut être utilisée pour capitaliser non seulement les lettres de l'alphabet, mais aussi les caractères spéciaux tels que les accents et les caractères Unicode.

De plus, si la chaîne de caractères comporte déjà des caractères en majuscule, la fonction `String.capitalize` ne les changera pas et se contentera de capitaliser le premier caractère non majuscule.

## Voir aussi 

- Documentation officielle de la fonction `String.capitalize` : https://package.elm-lang.org/packages/elm/core/latest/String#capitalize 
- Tutoriel Elm pour débutants : https://elmprogramming.com/ 
- Communauté Elm France : https://elm-france.org/