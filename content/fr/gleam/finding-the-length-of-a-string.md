---
title:    "Gleam: Trouver la longueur d'une chaîne."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

La programmation peut sembler compliquée et intimidante pour beaucoup, mais la réalité est que c'est en fait assez amusant et gratifiant une fois que vous commencez à en comprendre les concepts de base. L'une des choses les plus simples mais les plus utiles que vous pouvez apprendre est de trouver la longueur d'une chaîne de caractères en utilisant le langage de programmation Gleam.

## Comment faire

Voici un exemple de code Gleam montrant comment trouver la longueur d'une chaîne de caractères :

```Gleam
let string = "Bonjour le monde"
let length = String.length(string)
```

Dans cet exemple, nous avons une chaîne de caractères "Bonjour le monde" et nous utilisons la fonction interne `String.length()` pour trouver sa longueur. Lorsque nous imprimons la valeur de `length`, nous obtenons une sortie de 17 car il y a 17 caractères dans la chaîne y compris les espaces.

Vous pouvez également utiliser la boucle `for` pour imprimer les caractères individuels de la chaîne :

```Gleam
for char in string {
  IO.print(char)
}
```

Cela imprimera chaque caractère de la chaîne sur une ligne séparée. Vous pouvez également utiliser la fonction `String.slice()` pour extraire une sous-chaîne de la chaîne en utilisant des indices :

```Gleam
let sub_string = String.slice(string, 0, 7)
```

Cela extraira les 7 premiers caractères de la chaîne et les stockera dans la variable `sub_string`.

## Plongée en profondeur

Comprendre comment trouver la longueur d'une chaîne de caractères est important car cela vous permet de traiter efficacement et d'analyser des données sous forme de chaînes. Cela peut être utile dans le traitement de texte, la manipulation de données et de nombreux autres cas d'utilisation.

En Gleam, les chaînes de caractères sont des valeurs immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées. Cela garantit que les manipulations sur les chaînes de caractères ne modifient pas les valeurs d'origine, mais renvoient plutôt de nouvelles valeurs. Cela évite les erreurs et les confusions potentielles lors de la manipulation de données.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/)
- [Tutoriels de programmation en français](https://openclassrooms.com/fr/courses/2984401-apprenez-a-programmer-en-python)
- [Communauté Gleam sur Discord](https://discord.gg/QsfECDu)