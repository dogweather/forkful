---
title:    "Gleam: Conversion d'une chaîne en minuscules"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être un développeur débutant ou expérimenté, mais que vous n'avez jamais entendu parler de la conversion de chaîne en minuscules. Ou peut-être que vous avez travaillé avec d'autres langages de programmation qui ont des fonctions pour cela, mais vous êtes maintenant intéressé par Gleam et vous voulez savoir comment effectuer cette opération. Dans cet article, nous allons examiner en détail pourquoi et comment convertir une chaîne en minuscules en utilisant Gleam.

## Comment faire

Voici un exemple de code Gleam qui montre comment convertir une chaîne en minuscules:

```Gleam
fn string_to_lowercase(string) {
  lower = string.to_lower()
  io.println(lower)
}

fn main() {
  string_to_lowercase("EXEMPLE")
}

// Output: "exemple"
```

Dans cet exemple, nous avons déclaré une fonction `string_to_lowercase` qui prend une chaîne en tant que paramètre. Ensuite, nous utilisons la méthode `to_lower()` sur la chaîne pour la convertir en minuscules et l'imprimons à l'aide de la fonction `println` de Gleam. Dans la fonction `main`, nous appelons la fonction `string_to_lowercase` en lui passant la chaîne "EXEMPLE" comme argument. Lorsque le code est exécuté, le résultat sera "exemple".

Il est important de noter que la méthode `to_lower()` ne modifie pas la chaîne d'origine, mais renvoie plutôt une nouvelle chaîne en minuscules. Cela peut être utile si vous avez besoin de conserver la version originale en majuscules pour une utilisation ultérieure.

## Exploration en profondeur

Si vous voulez en savoir plus sur la conversion de chaîne en minuscules en utilisant Gleam, voici quelques points à retenir:

- Cette méthode utilise le standard Unicode pour convertir les caractères en minuscules, ce qui prend en compte les caractères spéciaux et les accents.

- Si vous essayez de convertir une chaîne vide en minuscules, la méthode renverra une chaîne vide.

- Si vous avez besoin de manipuler des caractères non ASCII dans votre chaîne, il est recommandé d'utiliser la bibliothèque `unicode` de Gleam qui offre des fonctions pour travailler avec ces caractères.

Maintenant que vous savez comment convertir une chaîne en minuscules en utilisant Gleam, vous pouvez l'appliquer dans vos projets et écrire du code plus propre et plus lisible.

## Voir aussi

- [Documentation officielle de la méthode to_lower()](https://gleam.run/documentation/stdlib/string#to_lower)

- [Bibliothèque unicode de Gleam](https://gleam.run/documentation/stdlib/unicode)