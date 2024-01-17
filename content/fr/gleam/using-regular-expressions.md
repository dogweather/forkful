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

## Quoi & Pourquoi?

L'utilisation d'expressions régulières est une pratique courante parmi les programmeurs. Cela leur permet de rechercher et de manipuler des motifs spécifiques dans du texte, ce qui peut s'avérer très utile lors de la validation de données ou du traitement de chaînes de caractères.

## Comment:

Voici un exemple simple d'utilisation des expressions régulières en Gleam:

```Gleam
let regex = Regex.compile("\\d{3}-\\d{2}-\\d{4}")
let ssn = "123-45-6789"

match regex.match(ssn) {
  Some(_) -> 
    // Code exécuté lorsque le numéro de sécurité sociale est valide
    //...
  None -> 
    // Code exécuté en cas d'erreur
    //...
}
```

Dans cet exemple, nous définissons une expression régulière pour vérifier si un numéro de sécurité sociale est valide. Ensuite, nous faisons correspondre le numéro de sécurité sociale fourni à cette expression régulière. Si la correspondance est réussie, le code à l'intérieur du bloc "Some" est exécuté, sinon le code à l'intérieur du bloc "None" est exécuté.

## Plongée en profondeur:

Les expressions régulières ont été initialement développées dans les années 1950 et sont devenues un outil essentiel pour les programmeurs dans de nombreux langages, y compris dans Gleam. Bien qu'elles puissent sembler intimidantes au premier abord, les expressions régulières peuvent être très puissantes une fois que l'on en comprend les bases.

Il existe également d'autres alternatives aux expressions régulières, comme les analyseurs lexicaux et syntaxiques, qui peuvent être plus adaptés à certains scénarios. Cependant, les expressions régulières restent un outil très utile et polyvalent pour manipuler et valider des données.

Pour comprendre comment les expressions régulières sont mises en œuvre en interne, vous pouvez jeter un coup d'œil au code source de Gleam qui utilise la bibliothèque Rust à cet effet.

## Voir aussi:

- Documentation sur les expressions régulières en Gleam: https://gleam.run/book/tour/regular-expressions.html
- Documentation officielle sur les expressions régulières en général: https://www.regular-expressions.info/