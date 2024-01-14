---
title:    "Elm: Recherche et remplacement de texte"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes-vous déjà retrouvé dans la situation où vous deviez remplacer du texte à plusieurs endroits dans votre code ? Cela peut être fastidieux et prendre beaucoup de temps, surtout si vous devez le faire manuellement. Heureusement, avec Elm, il existe une solution simple et efficace pour effectuer ces modifications rapidement et en toute simplicité.

# Comment Faire

Pour rechercher et remplacer du texte dans votre code Elm, il suffit d'utiliser la fonction `String.replace` avec une chaîne de caractères à remplacer et une chaîne de caractères de remplacement. Par exemple, si vous souhaitez remplacer toutes les occurrences de "Hello" par "Bonjour", vous pouvez utiliser la fonction de la manière suivante :

```Elm 
String.replace "Hello" "Bonjour" "Hello world!"
```

Cela renverra la chaîne de caractères "Bonjour world!" en remplaçant toutes les occurrences de "Hello" par "Bonjour". Vous pouvez également utiliser cette fonction pour remplacer du texte dans une variable, comme ceci :

```Elm 
let 
    variable = "Hello world!" 
in 
String.replace "Hello" "Bonjour" variable 
```

Cela remplacera également toutes les occurrences de "Hello" par "Bonjour" dans la variable. Vous pouvez également utiliser une chaîne de caractères de remplacement vide si vous souhaitez supprimer simplement le texte spécifié. Par exemple, si vous souhaitez supprimer toutes les occurrences de "Bonjour" dans une chaîne de caractères, vous pouvez utiliser la fonction avec une chaîne de caractères de remplacement vide : 

```Elm 
String.replace "Bonjour" "" "Bonjour à tous !"
```

Cela renverra simplement la chaîne de caractères vide, supprimant ainsi toutes les occurrences de "Bonjour".

# Plongée Approfondie 

La fonction `String.replace` est utile pour effectuer des modifications de base, mais elle peut également prendre en charge des schémas de recherche plus complexes. Par exemple, vous pouvez utiliser une expression régulière et une fonction de remplacement pour remplacer du texte. Voici un exemple :

```Elm 
String.replace (Regex.regex "Hello(.)?") (\_ -> "Bonjour") "Hello world!"
```

Cela utilise une expression régulière pour capturer le caractère suivant "Hello" et le remplace par "Bonjour". Dans cet exemple, cela remplacera "Hell" par "Bonjour".

# Voir Aussi

- [Documentation officielle pour la fonction `String.replace` dans Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Tutoriel sur les expressions régulières en Elm](https://www.elm-tutorial.org/en/05-regexes/)