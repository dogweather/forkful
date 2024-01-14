---
title:                "Gleam: Recherche et remplacement de texte"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans le développement de logiciels. Cela peut être utile pour changer des variables, corriger des fautes d'orthographe ou encore pour effectuer des modifications massives dans un fichier. Dans cet article, nous allons vous montrer comment utiliser Gleam pour simplifier ce processus.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en utilisant Gleam, vous pouvez utiliser la fonction `String.replace/3`. Cette fonction prend trois arguments : la chaine de caractères dans laquelle vous souhaitez effectuer le remplacement, la partie du texte à rechercher et la nouvelle valeur à y substituer. Par exemple :

```Gleam
let old_string = "Bonjour le monde!"
let new_string = String.replace(old_string, "Bonjour", "Salut")

// Output : "Salut le monde!"
```

Vous pouvez également utiliser les expressions régulières pour effectuer des remplacements plus avancés. La fonction `Regex.replace/4` vous permet de spécifier une expression régulière pour la partie du texte à rechercher et des fonctions de callback pour personnaliser la substitution.

```Gleam
let old_string = "Alice a 5 ans et Bob a 10 ans."
let new_string =
  Regex.replace(old_string, #"\d+", fn x ->
    let age = Integer.fromString(x, 10)
    case age {
      Ok(age) -> String.from_int(age * 2)
      Err(_) -> x
    }
  end)

// Output : "Alice a 10 ans et Bob a 20 ans."
```

## Plongée en profondeur

La fonction `String.replace/3` est efficace pour les remplacements simples, mais elle peut présenter des limites dans certains cas. Par exemple, elle ne permet pas de remplacer des caractères sensibles à la casse, comme les lettres accentuées en français. Dans ce cas, vous pouvez utiliser la fonction `String.replace_first/3` pour effectuer un remplacement en respectant la casse.

De plus, si vous avez besoin de remplacer des mots entiers et non des parties de mots, il est recommandé d'utiliser la fonction `String.words/1` pour diviser votre chaîne de caractères en mots et ainsi éviter de remplacer des parties de mots involontairement.

## Voir aussi

- [Documentation de Gleam pour la manipulation de chaînes de caractères](https://gleam.run/core/string.html)
- [Documentation de Gleam pour les expressions régulières](https://gleam.run/core/regex.html)
- [Liste des expressions régulières couramment utilisées en français pour la recherche et le remplacement de texte](https://www.bomel.ca/regex/liste.html)