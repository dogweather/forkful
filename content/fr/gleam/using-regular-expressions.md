---
title:                "Utiliser des expressions régulières"
html_title:           "Gleam: Utiliser des expressions régulières"
simple_title:         "Utiliser des expressions régulières"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Tu as sûrement déjà croisé des expressions régulières (ou *regular expressions*), surtout si tu es un.e développeur.se. Mais pourquoi se donner la peine d'apprendre ces petits bouts de code ? En un mot : efficacité. Les expressions régulières te permettent de manipuler rapidement et précisément du texte, sans avoir à écrire une tonne de lignes de code.

## Comment faire
Pour commencer, il faut savoir que les expressions régulières sont écrites entre deux barres obliques, comme ceci : `/expression regulière/`. Ensuite, il suffit de les utiliser avec les fonctions `Regex.matches` ou `Regex.replace`. Regardons un exemple :

```Gleam
let email_regex = Regex.compile("/^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$/")

let email = "john.doe@example.com"

let is_valid = Regex.matches(email_regex, email)

assert.equal(is_valid, True) // Renvoie True car l'email est valide selon l'expression régulière.
```

On peut également utiliser les expressions régulières pour remplacer une partie d'une chaîne de caractères. Par exemple :

```Gleam
let sentence = "J'aime les chats et les chiens."

let regex = Regex.compile("/chats/")

let new_sentence = Regex.replace(regex, sentence, "lapins")

assert.equal(new_sentence, "J'aime les lapins et les chiens.") // Renvoie "J'aime les lapins et les chiens."
```

## Plongée en profondeur
Si tu veux vraiment pousser tes connaissances en expressions régulières, il y a quelques éléments à savoir. Tout d'abord, les expressions régulières sont sensibles à la casse, c'est-à-dire qu'elles font la différence entre les lettres majuscules et minuscules. Pour ignorer la casse, il faut utiliser le drapeau `/i` après l'expression régulière.

De plus, il est possible d'utiliser des quantificateurs pour spécifier le nombre de fois qu'un motif doit apparaître. Par exemple, `+` indique qu'un caractère doit apparaître une ou plusieurs fois, tandis que `*` indique qu'il peut apparaître zéro ou plusieurs fois. Il existe également des quantificateurs de nombre précis, comme `{3}` pour indiquer qu'un caractère doit apparaître exactement trois fois.

Enfin, les expressions régulières peuvent être utilisées avec des classes de caractères, qui permettent de spécifier un ensemble de caractères autorisés. Par exemple, `[a-z]` indique que le caractère doit être une lettre minuscule.

## Voir aussi
- [La documentation officielle de Gleam](https://gleam.run/libraries/regex/)
- [Un guide complet sur les expressions régulières](https://regexone.com/)
- [Un testeur en ligne pour les expressions régulières](https://regex101.com/)