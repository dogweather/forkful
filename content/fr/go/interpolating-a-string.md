---
title:                "Interpoler une chaîne de caractères"
html_title:           "Go: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi les programmeurs le font?

Interpoler une chaîne de caractères (ou "string" en anglais) est un moyen de concaténer dynamiquement des variables ou des valeurs dans une chaîne de caractères. Les programmeurs le font pour rendre leurs chaînes de caractères plus flexibles et facilement modifiables en y incluant des valeurs variables.

Comment le faire:

```Go
name := "Bob"
age := 25
fmt.Printf("Salut, je m'appelle %s et j'ai %d ans.", name, age)
```

Résultat:

```
Salut, je m'appelle Bob et j'ai 25 ans.
```

Des détails approfondis:

L'interpolation de chaîne de caractères a été introduite dans Go en tant que fonctionnalité expérimentale dans la version 1.12. Elle est rapidement devenue populaire parmi les programmeurs pour sa simplicité et sa convivialité. Avant cela, les programmeurs devaient utiliser la fonction de formatage de chaîne de caractères "Sprintf" pour concaténer des variables avec une chaîne de caractères.

Pour les alternatives, certaines langues utilisent une syntaxe similaire pour interpoler des chaînes de caractères telles que Ruby et JavaScript avec la syntaxe " #{variable}" pour inclure une variable dans une chaîne de caractères.

En ce qui concerne l'implémentation technique, l'interpolation de chaîne de caractères utilise la fonction "Sprintf" dans les coulisses et est donc une simplification syntaxique pour les programmeurs.

Voir aussi:

Pour plus d'informations sur l'interpolation de chaîne de caractères en Go, vous pouvez consulter la documentation officielle de Go : https://golang.org/pkg/fmt/#pkg-overview